open Ppx_sexp_conv_lib.Conv
open EsyPackageConfig
module Solution = EsyInstall.Solution

module Id = struct
  type t =
    | Self
    | Root
    | ByName of string
    [@@deriving ord, sexp_of]

  let pp fmt = function
    | Self -> Fmt.unit "self" fmt ()
    | Root -> Fmt.unit "root" fmt ()
    | ByName name -> Fmt.string fmt name
end

include EsyInstall.DepSpec.Make(Id)

let root = Id.Root
let self = Id.Self
let name name = Id.ByName name

let resolve solution self id =
  match id with
  | Id.Root -> Some (Solution.root solution).id
  | Id.Self -> Some self
  | Id.ByName name ->
    begin match Solution.findByName name solution with
    | Some pkg -> Some pkg.id
    | None -> None
    end

let resolveOrError solution self id =
  match resolve solution self id with
  | None -> Run.errorf "no package found for %a expression" Id.pp id
  | Some id -> Run.return id

let eval solution self depspec =
  let open Run.Syntax in
  let resolve id = resolveOrError solution self id in
  let rec eval' expr =
    match expr with
    | Package id ->
      let%bind id = resolve id in
      return (PackageId.Set.singleton id)
    | Dependencies id ->
      let%bind id = resolve id in
      let pkg = Solution.getExn id solution in
      return pkg.dependencies
    | DevDependencies id ->
      let%bind id = resolve id in
      let pkg = Solution.getExn id solution in
      return pkg.devDependencies
    | Union (a, b) ->
      let%bind a = eval' a in
      let%bind b = eval' b in
      return (PackageId.Set.union a b)
  in
  eval' depspec

let rec collect' solution depspec seen id =
  let open Run.Syntax in
  if PackageId.Set.mem id seen
  then return seen
  else
    let seen = PackageId.Set.add id seen in
    let%bind ids = eval solution id depspec in
    let%bind seen =
      let f seen nextid =
        collect' solution depspec seen nextid
      in
      Run.List.foldLeft ~f ~init:seen (PackageId.Set.elements ids)
    in
    return seen

let collect solution depspec root =
  collect' solution depspec PackageId.Set.empty root
