open Kalk
open Alcotest

let expect_tokens text expected =
  Token.(
    match tokenize (init text) with
    | Error e -> fail (display_error e)
    | Ok t ->
        check (list string) "parse result"
          (List.map display expected)
          (List.map display t))

let expect_error text error =
  Token.(
    match tokenize (init text) with
    | Error e ->
        check string "parse error" (Token.display_error e)
          (Token.display_error error)
    | Ok t -> fail ("Expected to fail, received " ^ debug t))

let expect_result text expected =
  let result = exec text >>= AST.float_of_number in
  match result with
  | Ok n -> check (float epsilon_float) "exec error" n expected
  | Error e -> fail (Token.display_error e)

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
  expect_result "1 +2" 3.0

let () =
  let open Alcotest in
  run "Utils"
    [ ("string-case", [ test_case "Parser works" `Quick test_parser ]) ]
