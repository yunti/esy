type t =
  | OfJson of {json : Json.t;}
  | OfSource of {source : Source.t; json : Json.t;}
  | OfOpamOverride of {
      path : Path.t;
      json : Json.t;
    }

val pp : t Fmt.t

type build = {
  buildType : BuildType.t option;
  build : PackageConfig.CommandList.t option;
  install : PackageConfig.CommandList.t option;
  exportedEnv: PackageConfig.ExportedEnv.t option;
  exportedEnvOverride: PackageConfig.ExportedEnvOverride.t option;
  buildEnv: PackageConfig.Env.t option;
  buildEnvOverride: PackageConfig.EnvOverride.t option;
}

type install = {
  dependencies : PackageConfig.NpmFormulaOverride.t option;
  devDependencies : PackageConfig.NpmFormulaOverride.t option;
  resolutions : PackageConfig.Resolution.resolution StringMap.t option [@default None];
}

val build : t -> build option RunAsync.t
val install : t -> install option RunAsync.t

val ofJson : Json.t -> t
val ofSource : Json.t -> Source.t -> t

val files : Config.t -> SandboxSpec.t -> t -> File.t list RunAsync.t
