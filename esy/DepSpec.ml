include DepSpecImpl

let parse v =
  let open Result.Syntax in
  let lexbuf = Lexing.from_string v in
  try return (DepSpecParser.start DepSpecLexer.read lexbuf) with
  | DepSpecLexer.Error msg ->
    errorf "error parsing DEPSPEC: %s" msg
  | DepSpecParser.Error -> error "error parsing DEPSPEC"

let%test_module "parsing" = (module struct
  let parseAndPrint v =
    match parse v with
    | Error err ->
      Format.printf "ERROR: %s@." err
    | Ok v ->
      let sexp = sexp_of_t v in
      Format.printf "%a@." Sexplib0.Sexp.pp_hum sexp

  let%expect_test _ =
    parseAndPrint "self";
    [%expect {| (Package Self) |}]

  let%expect_test _ =
    parseAndPrint "root";
    [%expect {| (Package Root) |}]

  let%expect_test _ =
    parseAndPrint "dependencies(root)";
    [%expect {| (Dependencies Root) |}]

  let%expect_test _ =
    parseAndPrint "dependencies(self)";
    [%expect {| (Dependencies Self) |}]

  let%expect_test _ =
    parseAndPrint "devDependencies(root)";
    [%expect {| (DevDependencies Root) |}]

  let%expect_test _ =
    parseAndPrint "devDependencies(self)";
    [%expect {| (DevDependencies Self) |}]

  let%expect_test _ =
    parseAndPrint "dependencies(root) + devDependencies(self)";
    [%expect {| (Union (DevDependencies Self) (Dependencies Root)) |}]

  let%expect_test _ =
    parseAndPrint "dependencies(root) - devDependencies(self)";
    [%expect {| (Diff (Dependencies Root) (DevDependencies Self)) |}]

  let%expect_test _ =
    parseAndPrint "package('name')";
    [%expect {| (Package (ByName name)) |}]

  let%expect_test _ =
    parseAndPrint "package('name-x')";
    [%expect {| (Package (ByName name-x)) |}]

  let%expect_test _ =
    parseAndPrint "package('name_x')";
    [%expect {| (Package (ByName name_x)) |}]

  let%expect_test _ =
    parseAndPrint "package('@scope/name')";
    [%expect {| (Package (ByName @scope/name)) |}]

  let%expect_test _ =
    parseAndPrint "package('@scope-x/name')";
    [%expect {| (Package (ByName @scope-x/name)) |}]

  let%expect_test _ =
    parseAndPrint "dependencies(package('@scope/root')) + package('@scope/b')";
    [%expect {| (Union (Dependencies (ByName @scope/root)) (Package (ByName @scope/b))) |}]
end)
