(**

    Storage for package dists.

 *)

type archive
(** Fetched dist. *)

val fetch :
  cfg : Config.t
  -> Source.dist
  -> (archive, Run.error) result RunAsync.t
(** Fetch dist. *)

val unpack :
  cfg : Config.t
  -> dst : Path.t
  -> archive
  -> unit RunAsync.t
(** Unpack fetched archive in a specified directory. *)

val fetchAndUnpack :
  cfg : Config.t
  -> dst : Path.t
  -> Source.dist
  -> unit RunAsync.t
(** Shortcut for fetch & unpack *)
