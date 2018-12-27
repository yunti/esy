module type DEPSPEC = sig
  type id

  type t =
    private
    | Package of id
    | Dependencies of id
    | DevDependencies of id
    | Union of t * t
    | Diff of t * t
  (* term *)

  val package : id -> t
  (* refer to a package defined by source *)

  val dependencies : id -> t
  (* refer to dependencies defined by source *)

  val devDependencies : id -> t
  (* refer to devDependencies defined by source *)

  val union : t -> t -> t
  (** [union a b] produces a new term with all packages defined by [a] and * [b] *)

  val diff : t -> t -> t
  (** [diff a b] produces a new term with all packages in [a] which are not in [b] *)

  val (+) : t -> t -> t
  (** [a + b] is the same as [union a b] *)

  val (-) : t -> t -> t
  (** [a - b] is the same as [diff a b] *)

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module type ID = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Make (Id : ID) : DEPSPEC with type id = Id.t = struct

  type id = Id.t
  type t =
    | Package of Id.t
    | Dependencies of Id.t
    | DevDependencies of Id.t
    | Union of t * t
    | Diff of t * t
    [@@deriving ord, sexp_of]

  let package src = Package src
  let dependencies src = Dependencies src
  let devDependencies src = DevDependencies src

  let union a b =
    if compare a b > 0
    then Union (a, b)
    else Union (b, a)

  let diff a b = Diff (a, b)

  let (+) = union
  let (-) = diff

  let rec pp fmt spec =
    match spec with
    | Package id -> Fmt.pf fmt "%a" Id.pp id
    | Dependencies id -> Fmt.pf fmt "dependencies(%a)" Id.pp id
    | DevDependencies id -> Fmt.pf fmt "devDependencies(%a)" Id.pp id
    | Union (a, b) -> Fmt.pf fmt "%a+%a" pp a pp b
    | Diff (a, b) -> Fmt.pf fmt "%a-%a" pp a pp b
end
