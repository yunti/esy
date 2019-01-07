open EsyPackageConfig

module SandboxPath : module type of EsyBuildPackage.Config.Path
module SandboxValue : module type of EsyBuildPackage.Config.Value
module SandboxEnvironment : module type of EsyBuildPackage.Config.Environment

module Findlib : sig
  type t

  val name : prefix:SandboxPath.t -> t -> string
  val content : t -> string
end

type t

val make :
  platform:System.Platform.t
  -> sandboxEnv:SandboxEnvironment.Bindings.t
  -> id:BuildId.t
  -> name:string
  -> version:Version.t
  -> mode:BuildSpec.mode
  -> depspec:EsyInstall.Solution.DepSpec.t
  -> sourceType:SourceType.t
  -> sourcePath:SandboxPath.t
  -> EsyInstall.Package.t
  -> BuildManifest.t
  -> t
(** An initial scope for the package. *)

val add : direct:bool -> dep:t -> t -> t
(** Add new pkg *)

val pkg : t -> EsyInstall.Package.t
val id : t -> BuildId.t
val mode : t -> BuildSpec.mode
val depspec : t -> EsyInstall.Solution.DepSpec.t
val name : t -> string
val version : t -> Version.t
val sourceType : t -> SourceType.t
val buildType : t -> BuildType.t
val storePath : t -> SandboxPath.t
val rootPath : t -> SandboxPath.t
val sourcePath : t -> SandboxPath.t
val buildPath : t -> SandboxPath.t
val buildInfoPath : t -> SandboxPath.t
val stagePath : t -> SandboxPath.t
val installPath : t -> SandboxPath.t
val logPath : t -> SandboxPath.t

val pp : t Fmt.t

val env :
  includeBuildEnv:bool
  -> buildIsInProgress:bool
  -> t
  -> SandboxEnvironment.Bindings.t Run.t

val render :
  ?env:SandboxEnvironment.t
  -> ?environmentVariableName:string
  -> buildIsInProgress:bool
  -> t
  -> string
  -> SandboxValue.t Run.t

val toOpamEnv : buildIsInProgress:bool -> t -> OpamFilter.env

val exposeUserEnvWith : (string -> SandboxValue.t -> SandboxValue.t Environment.Binding.t) -> string -> t -> t

val toFindlibConfig: t -> Findlib.t list
