type override =
  | OfJson of {json : Json.t}
  | OfPath of Dist.local
  | OfLink of Dist.local
  | OfOpamOverride of {path : DistPath.t;}

let override_to_yojson override =
  match override with
  | OfJson {json;} -> json
  | OfPath local -> `Assoc ["path", Dist.local_to_yojson local;]
  | OfLink local -> `Assoc ["link", Dist.local_to_yojson local;]
  | OfOpamOverride { path; } -> `Assoc ["opam", DistPath.to_yojson path;]

let override_of_yojson json =
  let open Result.Syntax in
  match json with
  | `Assoc ["path", json;] ->
    let%bind local = Dist.local_of_yojson json in
    return (OfPath local)
  | `Assoc ["link", json;] ->
    let%bind local = Dist.local_of_yojson json in
    return (OfLink local)
  | `Assoc ["opam", path;] ->
    let%bind path = DistPath.of_yojson path in
    return (OfOpamOverride {path;})
  | `Assoc _ ->
    return (OfJson {json;})
  | _ -> error "expected a string or an object"

type overrides = override list [@@deriving yojson]

type t = {
  (* This is checksum of all dependencies/resolutios, used as a checksum. *)
  checksum : string;
  (* Id of the root package. *)
  root : PackageId.t;
  (* Map from ids to nodes. *)
  node : node PackageId.Map.t
} [@@deriving yojson]

and node = {
  id: PackageId.t;
  name: string;
  version: Version.t;
  source: PackageSource.t;
  override: overrides;
  dependencies : PackageId.Set.t;
  devDependencies : PackageId.Set.t;
}

let indexFilename = "index.json"

let gitAttributesContents = {|
# Set eol to LF so files aren't converted to CRLF-eol on Windows.
* text eol=lf
|}

let gitIgnoreContents = {|
# Reset any possible .gitignore, we want all esy.lock to be un-ignored.
!*
|}

module PackageOverride = struct
  type t = {
    override : Json.t;
  } [@@deriving of_yojson {strict = false}]

  let ofPath path =
    let open RunAsync.Syntax in
    RunAsync.contextf (
      let%bind json = Fs.readJsonFile path in
      let%bind data = RunAsync.ofRun (Json.parseJsonWith of_yojson json) in
      return data.override
    ) "reading package override %a" Path.pp path
end

let writeOverride sandbox pkg override =
  let open RunAsync.Syntax in
  let rec write override =
    match override with
    | Override.Empty -> return []
    | Override.Override { json; prev; kind = Json; } ->
      let%bind rest = write prev in
      return (OfJson {json;}::rest)
    | Override.Override { json = _; prev; kind = Opam path; } ->
      let%bind rest = write prev in
      let id =
        Format.asprintf "%s-%a-opam-override"
          pkg.Package.name
          Version.pp
          pkg.version
      in
      let lockPath = Path.(
        SandboxSpec.solutionLockPath sandbox.Sandbox.spec
        / "overrides"
        / Path.safeSeg id
      ) in
      let%bind () = Fs.copyPath ~src:path ~dst:lockPath in
      let path = DistPath.ofPath (Path.tryRelativize ~root:sandbox.spec.path lockPath) in
      return (OfOpamOverride {path;}::rest)
    | Override.Override { json = _; prev; kind = Source (Source.Link local); } ->
      let%bind rest = write prev in
      return (OfLink local::rest)
    | Override.Override { json = _; prev; kind = Source (Source.Dist dist); } ->
      let%bind rest = write prev in
      let%bind distPath = DistStorage.fetchIntoCache ~cfg:sandbox.cfg ~sandbox:sandbox.spec dist in
      let digest = Digestv.ofString (Dist.show dist) in
      let lockPath = Path.(
        SandboxSpec.solutionLockPath sandbox.Sandbox.spec
        / "overrides"
        / Digestv.toHex digest
      ) in
      let%bind () = Fs.copyPath ~src:distPath ~dst:lockPath in
      let manifest = Dist.manifest dist in
      let path = DistPath.ofPath (Path.tryRelativize ~root:sandbox.spec.path lockPath) in
      return (OfPath {path; manifest}::rest)
  in
  write override

let readOverride sandbox nodes =
  let open RunAsync.Syntax in
  let readJsonOfLocal (local : Dist.local) =
    let filename =
      match local.manifest with
      | None -> "package.json"
      | Some One (Esy, filename) -> filename
      | Some One (Opam, _filename) -> failwith "cannot load override from opam file"
      | Some ManyOpam -> failwith "cannot load override from opam files"
    in
    let path = DistPath.toPath sandbox.Sandbox.spec.path DistPath.(local.path / filename) in
    let%bind json = PackageOverride.ofPath path in
    return json
  in
  let f override node =
    match node with
    | OfJson {json;} ->
      let override = Override.ofJson json override in
      return override
    | OfOpamOverride {path;} ->
      let path = DistPath.toPath sandbox.Sandbox.spec.path path in
      let%bind json = Fs.readJsonFile Path.(path / "package.json") in
      return (Override.ofOpam json path override)
    | OfPath local ->
      let dist = Dist.LocalPath local in
      let%bind json = readJsonOfLocal local in
      return (Override.ofSource json (Source.Dist dist) override)
    | OfLink local ->
      let%bind json = readJsonOfLocal local in
      return (Override.ofSource json (Source.Link local) override)
  in
  RunAsync.List.foldLeft ~f ~init:Override.Empty nodes

let writeOpam sandbox (opam : PackageSource.opam) =
  let open RunAsync.Syntax in
  let sandboxPath = sandbox.Sandbox.spec.path in
  let opampath = Path.(sandboxPath // opam.path) in
  let dst =
    let name = OpamPackage.Name.to_string opam.name in
    let version = OpamPackage.Version.to_string opam.version in
    Path.(SandboxSpec.solutionLockPath sandbox.spec / "opam" / (name ^ "." ^ version))
  in
  if Path.isPrefix sandboxPath opampath
  then return opam
  else (
    let%bind () = Fs.copyPath ~src:opam.path ~dst in
    return {opam with path = Path.tryRelativize ~root:sandboxPath dst;}
  )

let readOpam sandbox (opam : PackageSource.opam) =
  let open RunAsync.Syntax in
  let sandboxPath = sandbox.Sandbox.spec.path in
  let opampath = Path.(sandboxPath // opam.path) in
  return {opam with path = opampath;}

let writePackage sandbox (pkg : Package.t) =
  let open RunAsync.Syntax in
  let%bind source =
    match pkg.source with
    | Link { path; manifest } -> return (PackageSource.Link {path; manifest;})
    | Install {source; opam = None;} -> return (PackageSource.Install {source; opam = None;})
    | Install {source; opam = Some opam;} ->
      let%bind opam = writeOpam sandbox opam in
      return (PackageSource.Install {source; opam = Some opam;});
  in
  let%bind override = writeOverride sandbox pkg pkg.override in
  return {
    id = pkg.id;
    name = pkg.name;
    version = pkg.version;
    source;
    override;
    dependencies = pkg.dependencies;
    devDependencies = pkg.devDependencies;
  }

let readPackage sandbox (node : node) =
  let open RunAsync.Syntax in
  let%bind source =
    match node.source with
    | Link { path; manifest } -> return (PackageSource.Link {path;manifest;})
    | Install {source; opam = None;} -> return (PackageSource.Install {source; opam = None;})
    | Install {source; opam = Some opam;} ->
      let%bind opam = readOpam sandbox opam in
      return (PackageSource.Install {source; opam = Some opam;});
  in
  let%bind override = readOverride sandbox node.override in
  return {
    Package.
    id = node.id;
    name = node.name;
    version = node.version;
    source;
    override;
    dependencies = node.dependencies;
    devDependencies = node.devDependencies;
  }

let solutionOfLock sandbox root node =
  let open RunAsync.Syntax in
  let f _id node solution =
    let%bind solution = solution in
    let%bind pkg = readPackage sandbox node in
    return (Solution.add pkg solution)
  in
  PackageId.Map.fold f node (return (Solution.empty root))

let lockOfSolution sandbox (solution : Solution.t) =
  let open RunAsync.Syntax in
  let%bind node =
    let f pkg _dependencies nodes =
      let%bind nodes = nodes in
      let%bind node = writePackage sandbox pkg in
      return (
        PackageId.Map.add
          pkg.Package.id
          node
          nodes)
    in
    Solution.fold ~f ~init:(return PackageId.Map.empty) solution
  in
  return (Solution.root solution, node)

let ofPath ~checksum ~(sandbox : Sandbox.t) (path : Path.t) =
  let open RunAsync.Syntax in
  RunAsync.contextf (
    Logs_lwt.debug (fun m -> m "SolutionLock.ofPath %a" Path.pp path);%lwt
    if%bind Fs.exists path
    then
      let%lwt lock =
        let%bind json = Fs.readJsonFile Path.(path / indexFilename) in
        RunAsync.ofRun (Json.parseJsonWith of_yojson json)
      in
      match lock with
      | Ok lock ->
        if String.compare lock.checksum checksum = 0
        then
          let%bind solution = solutionOfLock sandbox lock.root lock.node in
          return (Some solution)
        else return None
      | Error err ->
        let path =
          Option.orDefault
            ~default:path
            (Path.relativize ~root:sandbox.spec.path path)
        in
        errorf
          "corrupted %a lock@\nyou might want to remove it and install from scratch@\nerror: %a"
          Path.pp path Run.ppError err
    else
      return None
  ) "reading lock %a" Path.pp path

let toPath ~checksum ~sandbox ~(solution : Solution.t) (path : Path.t) =
  let open RunAsync.Syntax in
  Logs_lwt.debug (fun m -> m "SolutionLock.toPath %a" Path.pp path);%lwt
  let%bind () = Fs.rmPath path in
  let%bind root, node = lockOfSolution sandbox solution in
  let lock = {checksum; node; root = root.Package.id;} in
  let%bind () = Fs.createDir path in
  let%bind () = Fs.writeJsonFile ~json:(to_yojson lock) Path.(path / indexFilename) in
  let%bind () = Fs.writeFile ~data:gitAttributesContents Path.(path / ".gitattributes") in
  let%bind () = Fs.writeFile ~data:gitIgnoreContents Path.(path / ".gitignore") in
  return ()

let unsafeUpdateChecksum ~checksum path =
  let open RunAsync.Syntax in
  let%bind lock =
    let%bind json = Fs.readJsonFile Path.(path / indexFilename) in
    RunAsync.ofRun (Json.parseJsonWith of_yojson json)
  in
  let lock = {lock with checksum;} in
  Fs.writeJsonFile ~json:(to_yojson lock) Path.(path / indexFilename)
