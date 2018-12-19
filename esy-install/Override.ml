type t =
  | Empty
  | Override of {prev : t; json : Json.t; kind : kind;}

and kind =
  | Json
  | Source of Source.t
  | Opam of Path.t

let pp fmt = function
  | Empty -> Fmt.unit "<no override>" fmt ()
  | Override {kind = Json; _} -> Fmt.unit "<inline override>" fmt ()
  | Override {kind = Source source; _} -> Fmt.pf fmt "override:%a" Source.pp source
  | Override {kind = Opam path; _} -> Fmt.pf fmt "opam-override:%a" Path.pp path

let ofJson json prev = Override {json; prev; kind = Json;}
let ofSource json source prev = Override {json; prev; kind = Source source;}
let ofOpam json path prev = Override {json; prev; kind = Opam path;}

let isEmpty = function
  | Empty -> true
  | Override _ -> false

let isLink override =
  let rec aux =
    function
    | Empty -> false
    | Override { kind = Json; prev; _ }
    | Override { kind = Opam _; prev; _ }
    | Override { kind = Source Source.Dist _; prev; _ } -> aux prev
    | Override { kind = Source Source.Link _; _ } -> true
  in
  aux override

let fold' ~f ~init override =
  let open Run.Syntax in
  let rec aux value override =
    match override with
    | Empty -> return value
    | Override {json; prev; kind;} ->
      let%bind value = aux value prev in
      let%bind value = f value json kind in
      return value
  in
  aux init override

let files' collectOf override =
  let open RunAsync.Syntax in

  let rec collect files override =
    match override with
    | Empty -> return files
    | Override {json = _; prev; kind ;} ->
      let%bind files = collect files prev in
      let%bind thisFiles = collectOf kind in
      return (files @ thisFiles)
  in

  collect [] override

let files cfg sandbox override =
  let open RunAsync.Syntax in
  let collectOfKind kind =
    match kind with
    | Json -> return []
    | Source Dist dist ->
      let%bind path = DistStorage.fetchIntoCache ~cfg ~sandbox dist in
      File.ofDir Path.(path / "files")
    | Source Link local ->
      let path = DistPath.toPath sandbox.path local.path in
      File.ofDir Path.(path / "files")
    | Opam path ->
      File.ofDir Path.(path / "files")
  in
  files' collectOfKind override

module Build = struct

  module BuildType = struct
    include EsyLib.BuildType
    include EsyLib.BuildType.AsInPackageJson
  end

  type override = {
    buildType : BuildType.t option [@default None] [@key "buildsInSource"];
    build : PackageConfig.CommandList.t option [@default None];
    install : PackageConfig.CommandList.t option [@default None];
    exportedEnv: PackageConfig.ExportedEnv.t option [@default None];
    exportedEnvOverride: PackageConfig.ExportedEnvOverride.t option [@default None];
    buildEnv: PackageConfig.Env.t option [@default None];
    buildEnvOverride: PackageConfig.EnvOverride.t option [@default None];
  } [@@deriving of_yojson { strict = false }]

  let fold ~f ~init override =
    let open Run.Syntax in
    let f v json _kind =
      let%bind override = Run.ofStringError (override_of_yojson json) in
      f v override
    in
    fold' ~f ~init override

end

module Install = struct

  type override = {
    dependencies : PackageConfig.NpmFormulaOverride.t option [@default None];
    devDependencies : PackageConfig.NpmFormulaOverride.t option [@default None];
    resolutions : PackageConfig.Resolution.resolution StringMap.t option [@default None];
  } [@@deriving of_yojson { strict = false }]

  let fold ~f ~init override =
    let open Run.Syntax in
    let f v json _kind =
      let%bind override = Run.ofStringError (override_of_yojson json) in
      f v override
    in
    fold' ~f ~init override

end
