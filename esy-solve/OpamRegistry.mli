(**
 * API for querying opam registry.
 *)

type t

val make : cfg:Config.t -> unit -> t
(** Configure a new opam registry instance. *)

val versions :
  ?ocamlVersion : EsyInstall.OpamPackageVersion.Version.t
  -> name : OpamPackage.Name.t
  -> t
  -> OpamResolution.t list RunAsync.t
(** Return a list of resolutions for a given opam package name. *)

val package :
  OpamPackage.Name.t
  -> OpamPackage.Version.t
  -> t
  -> (Package.t, string) result RunAsync.t
(** Return Package.t for given opam package name and version *)
