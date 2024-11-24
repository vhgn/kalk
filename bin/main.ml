open Kalk

let rec repl () =
  let text = read_line () in
  match text with
  | "exit" -> ()
  | _ ->
      let response =
        match exec text with
        | Ok num -> Common.display_num num
        | Error err -> Common.display_error err
      in
      print_endline response;
      repl ()

let () =
  print_endline "Hello, World! (kalk)";
  repl ()
