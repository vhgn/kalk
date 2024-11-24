open Kalk
open Alcotest

let expect_tokens text expected =
  Common.(
    match Token.tokenize text with
    | Error e -> fail (display_error e)
    | Ok t ->
        check (list string) "parse result"
          (List.map Token.display expected)
          (List.map Token.display t))

let expect_error text error =
  Common.(
    Token.(
      match tokenize text with
      | Error e ->
          check string "parse error" (display_error e) (display_error error)
      | Ok t -> fail ("Expected to fail, received " ^ debug t)))

let expect_ast text expected =
  Common.(
    AST.(
      Token.(
        let ast = tokenize text >>= parse in
        match ast with
        | Ok n ->
            check string "parse ast" (display_node n) (display_node expected)
        | Error e -> fail (display_error e))))

let expect_result text expected =
  Common.(
    let result = exec text |> Result.map float_of_num in
    match result with
    | Ok n -> check (float epsilon_float) "exec error" n expected
    | Error e -> fail (display_error e))

let test_parser _ =
  expect_tokens "112" [ Number [ '1'; '1'; '2' ] ];
  expect_tokens "1+2" [ Number [ '1' ]; Operator Plus; Number [ '2' ] ];
  expect_tokens "(1+2)"
    [ LBrace; Number [ '1' ]; Operator Plus; Number [ '2' ]; RBrace ];
  expect_tokens "1+2 * 443 -    3"
    [
      Number [ '1' ];
      Operator Plus;
      Number [ '2' ];
      Operator Multiply;
      Number [ '4'; '4'; '3' ];
      Operator Minus;
      Number [ '3' ];
    ];
  expect_error "1+ _ " (`Character_not_supp '_');
  expect_ast "1 +2" (Expression (Number (Int 1), Plus, Number (Int 2)));
  expect_result "1 +2" 3.0

let () =
  let open Alcotest in
  run "Utils"
    [ ("string-case", [ test_case "Parser works" `Quick test_parser ]) ]
