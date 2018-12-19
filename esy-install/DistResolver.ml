module PackageOverride = struct
  type t = {
    source : Source.t;
    override : Json.t;
  }

  let of_yojson json =
    let open Result.Syntax in
    let%bind source =
      Json.Decode.fieldWith
        ~name:"source"
        Source.relaxed_of_yojson
        json
    in
    let%bind override =
      Json.Decode.fieldWith
      ~name:"override"
      Json.of_yojson
      json
    in
    return {source; override;}

end

type resolution = {
  override : Override.t;
  source : Source.t;
  manifest : manifest option;
  paths : Path.Set.t;
}

and manifest = {
  kind : ManifestSpec.Filename.kind;
  filename : string;
  suggestedPackageName : string;
  data : string;
}

type state =
  | EmptyManifest
  | Manifest of manifest
  | Override of PackageOverride.t

let rebase ~(base : Source.t) (source : Source.t) =
  let open Run.Syntax in
  match source, base with
  | Dist Dist.LocalPath info, Link {path = basePath; _} ->
    let path = DistPath.rebase ~base:basePath info.path in
    return (Source.Dist (Dist.LocalPath {info with path;}))
  | Dist Dist.LocalPath info, Dist Dist.LocalPath {path = basePath; _} ->
    let path = DistPath.rebase ~base:basePath info.path in
    return (Source.Dist (Dist.LocalPath {info with path;}))
  | Dist Dist.LocalPath _, _
  | Link _, Dist _
  | Link _, Link _ ->
    errorf "unable to rebase %a onto %a" Source.pp source Source.pp base
  | source, _ -> return source

let suggestPackageName ~fallback (kind, filename) =
  let ensurehasOpamScope name =
    match Astring.String.cut ~sep:"@opam/" name with
    | Some ("", _) -> name
    | Some _
    | None -> "@opam/" ^ name
  in
  let name =
    match ManifestSpec.Filename.inferPackageName (kind, filename) with
    | Some name -> name
    | None -> fallback
  in
  match kind with
  | ManifestSpec.Filename.Esy -> name
  | ManifestSpec.Filename.Opam -> ensurehasOpamScope name

let ofGithub
  ?manifest
  user
  repo
  ref =
  let open RunAsync.Syntax in
  let fetchFile name =
    let url =
      Printf.sprintf
        "https://raw.githubusercontent.com/%s/%s/%s/%s"
        user repo ref name
    in
    Curl.get url
  in

  let rec tryFilename filenames =
    match filenames with
    | [] -> return EmptyManifest
    | (kind, filename)::rest ->
      begin match%lwt fetchFile filename with
      | Error _ -> tryFilename rest
      | Ok data ->
        match kind with
        | ManifestSpec.Filename.Esy ->
          begin match Json.parseStringWith PackageOverride.of_yojson data with
          | Ok override -> return (Override override)
          | Error err ->
            let suggestedPackageName = suggestPackageName ~fallback:repo (kind, filename) in
            Logs_lwt.debug (fun m ->
              m "not an override %s/%s:%s: %a" user repo filename Run.ppError err
              );%lwt
            return (Manifest {data; filename; kind; suggestedPackageName;})
          end
        | ManifestSpec.Filename.Opam ->
          let suggestedPackageName = suggestPackageName ~fallback:repo (kind, filename) in
          return (Manifest {data; filename; kind; suggestedPackageName;})
      end
  in

  let filenames =
    match manifest with
    | Some manifest -> [manifest]
    | None -> [
      ManifestSpec.Filename.Esy, "esy.json";
      ManifestSpec.Filename.Esy, "package.json"
    ]
  in

  tryFilename filenames

let ofPath ?manifest (path : Path.t) =
  let open RunAsync.Syntax in

  let readManifest (kind, filename) manifestPath =
    let suggestedPackageName =
      suggestPackageName
        ~fallback:(Path.(path |> normalize |> remEmptySeg |> basename))
        (kind, filename)
    in

    if%bind Fs.exists manifestPath
    then
      let%bind data = Fs.readFile manifestPath in
      match kind with
      | ManifestSpec.Filename.Esy ->
        begin match Json.parseStringWith PackageOverride.of_yojson data with
        | Ok override -> return (Some (Override override))
        | Error err ->
          Logs_lwt.debug (fun m ->
            m "not an override %a: %a" Path.pp path Run.ppError err
            );%lwt
          return (Some (Manifest {data; filename; kind; suggestedPackageName;}))
        end
      | ManifestSpec.Filename.Opam ->
        return (Some (Manifest {data; filename; kind; suggestedPackageName;}))
    else
      return None
  in

  let rec tryManifests tried filenames =
    match filenames with
    | [] -> return (tried, EmptyManifest)
    | (kind, filename)::rest ->
      let manifestPath = Path.(path / filename) in
      let tried = Path.Set.add manifestPath tried in
      begin match%bind readManifest (kind, filename) manifestPath with
      | None -> tryManifests tried rest
      | Some state -> return (tried, state)
      end
  in

  match manifest with
  | Some manifest ->
    let%bind tried, state =
      let%bind filenames = ManifestSpec.findManifestsAtPath path manifest in
      tryManifests Path.Set.empty filenames
    in
    begin match state with
    | EmptyManifest ->
      errorf "unable to read manifests from %a" ManifestSpec.pp manifest
    | state ->
      return (tried, state)
    end
  | None ->
    tryManifests Path.Set.empty [
      ManifestSpec.Filename.Esy, "esy.json";
      ManifestSpec.Filename.Esy, "package.json";
      ManifestSpec.Filename.Opam, "opam";
      ManifestSpec.Filename.Opam, (Path.basename path ^ ".opam");
    ]

let resolve
  ~override
  ~cfg
  ~sandbox
  (source : Source.t) =
  let open RunAsync.Syntax in

  let resolve' (source : Source.t) =
    Logs_lwt.debug (fun m -> m "fetching metadata %a" Source.pp source);%lwt
    match source with
    | Link {path; manifest} ->
      let realpath = DistPath.toPath sandbox.SandboxSpec.path path in
      begin match%bind Fs.exists realpath with
      | false -> errorf "%a doesn't exist" DistPath.pp path
      | true ->
        let%bind tried, pkg = ofPath ?manifest realpath in
        return (pkg, tried)
      end
    | Dist LocalPath {path; manifest} ->
      let realpath = DistPath.toPath sandbox.SandboxSpec.path path in
      begin match%bind Fs.exists realpath with
      | false -> errorf "%a doesn't exist" DistPath.pp path
      | true ->
        let%bind tried, pkg = ofPath ?manifest realpath in
        return (pkg, tried)
      end
    | Dist Git {remote; commit; manifest;} ->
      let manifest = Option.map ~f:(fun m -> ManifestSpec.One m) manifest in
      Fs.withTempDir begin fun repo ->
        let%bind () = Git.clone ~dst:repo ~remote () in
        let%bind () = Git.checkout ~ref:commit ~repo () in
        let%bind _, pkg = ofPath ?manifest repo in
        return (pkg, Path.Set.empty)
      end
    | Dist Github {user; repo; commit; manifest;} ->
      let%bind pkg = ofGithub ?manifest user repo commit in
      return (pkg, Path.Set.empty)
    | Dist (Archive _ as dist) ->
      let%bind path =
        DistStorage.fetchIntoCache
          ~cfg
          ~sandbox
          dist
      in
      let%bind _, pkg = ofPath path in
      return (pkg, Path.Set.empty)

    | Dist NoSource ->
      return (EmptyManifest, Path.Set.empty)
  in

  let rec loop' ~override ~paths source =
    match%bind resolve' source with
    | EmptyManifest, newPaths ->
      return {
        manifest = None;
        override;
        source;
        paths = Path.Set.union paths newPaths;
      }
    | Manifest manifest, newPaths ->
      return {
        manifest = Some manifest;
        override;
        source;
        paths = Path.Set.union paths newPaths;
      }
    | Override {source = nextSource; override = json;}, newPaths ->
      let override = Override.ofSource json source override in
      let%bind nextDist = RunAsync.ofRun (rebase ~base:source nextSource) in
      Logs_lwt.debug (fun m -> m "override: %a -> %a@." Source.pp source Source.pp nextSource);%lwt
      let paths = Path.Set.union paths newPaths in
      loop' ~override ~paths nextDist
  in

  loop' ~override ~paths:Path.Set.empty source
