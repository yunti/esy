type t =
  | OfJson of {json : Json.t;}
  | OfSource of {source : Source.t; json : Json.t;}
  | OfOpamOverride of {
      path : Path.t;
      json : Json.t;
    }

module BuildType = struct
  include EsyLib.BuildType
  include EsyLib.BuildType.AsInPackageJson
end

type build = {
  buildType : BuildType.t option [@default None] [@key "buildsInSource"];
  build : PackageConfig.CommandList.t option [@default None];
  install : PackageConfig.CommandList.t option [@default None];
  exportedEnv: PackageConfig.ExportedEnv.t option [@default None];
  exportedEnvOverride: PackageConfig.ExportedEnvOverride.t option [@default None];
  buildEnv: PackageConfig.Env.t option [@default None];
  buildEnvOverride: PackageConfig.EnvOverride.t option [@default None];
} [@@deriving of_yojson { strict = false }]

type install = {
  dependencies : PackageConfig.NpmFormulaOverride.t option [@default None];
  devDependencies : PackageConfig.NpmFormulaOverride.t option [@default None];
  resolutions : PackageConfig.Resolution.resolution StringMap.t option [@default None];
} [@@deriving of_yojson { strict = false }]

let pp fmt = function
  | OfJson _ -> Fmt.unit "<inline override>" fmt ()
  | OfSource {source; json = _;} -> Fmt.pf fmt "override:%a" Source.pp source
  | OfOpamOverride info -> Fmt.pf fmt "opam-override:%a" Path.pp info.path

let json override =
  let open RunAsync.Syntax in
  match override with
  | OfJson info -> return info.json
  | OfSource info -> return info.json
  | OfOpamOverride info -> return info.json

let build override =
  let open RunAsync.Syntax in
  let%bind json = json override in
  let%bind override = RunAsync.ofStringError (build_of_yojson json) in
  return (Some override)

let install override =
  let open RunAsync.Syntax in
  let%bind json = json override in
  let%bind override = RunAsync.ofStringError (install_of_yojson json) in
  return (Some override)

let ofJson json = OfJson {json;}
let ofSource json source = OfSource {json; source;}

let files cfg sandbox override =
  let open RunAsync.Syntax in

  match override with
  | OfJson _ -> return []
  | OfSource { source = Source.Dist dist; json = _; } ->
    let%bind path = DistStorage.fetchIntoCache ~cfg ~sandbox dist in
    File.ofDir Path.(path / "files")
  | OfSource { source = Source.Link local; json = _; } ->
    let path = DistPath.toPath sandbox.path local.path in
    File.ofDir Path.(path / "files")
  | OfOpamOverride info ->
    File.ofDir Path.(info.path / "files")
