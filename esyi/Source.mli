type t =
  | Link of link
  | Dist of dist

and link = {
  path : Path.t;
  manifest : ManifestSpec.t option;
}

and dist =
  | Archive of {
      url : string;
      checksum : Checksum.t;
    }
  | Git of {
      remote : string;
      commit : string;
      manifest : ManifestSpec.Filename.t option;
    }
  | Github of {
      user : string;
      repo : string;
      commit : string;
      manifest : ManifestSpec.Filename.t option;
    }
  | LocalPath of {
      path : Path.t;
      manifest : ManifestSpec.t option;
    }
  | LocalPathLink of {
      path : Path.t;
      manifest : ManifestSpec.t option;
    }
  | NoSource

include S.COMMON with type t := t

val compare_link : link -> link -> int

val compare_dist : dist -> dist -> int
val show_dist : dist -> string
val pp_dist : dist Fmt.t
val pp_distPretty : dist Fmt.t
val dist_of_yojson : dist Json.decoder
val relaxed_dist_of_yojson : dist Json.decoder
val dist_to_yojson : dist Json.encoder

val relaxed_of_yojson : t Json.decoder

val sexp_of_t : t -> Sexplib0.Sexp.t
val ppPretty : t Fmt.t

val parser : t Parse.t
val parse : string -> (t, string) result

val parserRelaxed : t Parse.t
val parseRelaxed : string -> (t, string) result

val manifest : t -> ManifestSpec.t option

module Map : Map.S with type key := t
module Set : Set.S with type elt := t
