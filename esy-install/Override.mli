type t =
  | Empty
  | Override of {prev : t; json : Json.t; kind : kind;}

and kind =
  | Json
  | Source of Source.t
  | Opam of Path.t

val pp : t Fmt.t

val isEmpty : t -> bool

val ofJson : Json.t -> t -> t
val ofSource : Json.t -> Source.t -> t -> t
val ofOpam : Json.t -> Path.t -> t -> t

val files : Config.t -> SandboxSpec.t -> t -> File.t list RunAsync.t

module Build : sig
  type override = {
    buildType : BuildType.t option;
    build : PackageConfig.CommandList.t option;
    install : PackageConfig.CommandList.t option;
    exportedEnv : PackageConfig.ExportedEnv.t option;
    exportedEnvOverride : PackageConfig.ExportedEnvOverride.t option;
    buildEnv : PackageConfig.Env.t option;
    buildEnvOverride : PackageConfig.EnvOverride.t option;
  }

  val fold : f:('a -> override -> 'a Run.t) -> init:'a -> t -> 'a Run.t
end

module Install : sig
  type override = {
    dependencies : PackageConfig.NpmFormulaOverride.t option;
    devDependencies : PackageConfig.NpmFormulaOverride.t option;
    resolutions : PackageConfig.Resolution.resolution StringMap.t option;
  }

  val fold : f:('a -> override -> 'a Run.t) -> init:'a -> t -> 'a Run.t
end
