let ( let* ) = Result.bind
let ( >>= ) = Result.bind

module Common = struct
  type operator = Plus | Minus | Divide | Multiply
  type error = [ `Character_not_supp of char | `Double_dot_in_number ]
  type number = Int of int | Float of float

  let float_of_num n = match n with Int i -> float_of_int i | Float f -> f

  let operator_bigger l r =
    let operator_level op =
      match op with Plus -> 1 | Minus -> 1 | Multiply -> 2 | Divide -> 2
    in

    operator_level l > operator_level r

  let display_op op =
    match op with Plus -> "+" | Minus -> "-" | Multiply -> "*" | Divide -> "/"

  let display_num n =
    match n with Float f -> string_of_float f | Int i -> string_of_int i

  let display_error error =
    match error with
    | `Character_not_supp c -> "Coll other " ^ Char.escaped c
    | `Double_dot_in_number -> "Dot is two times in a number"
    | `Not_impl -> "Not implemented yet"
end

module Token = struct
  open Common

  type token =
    | Number of char list
    | Operator of operator
    | LBrace
    | RBrace
    | Ident of char list

  type collecting =
    | Numbers of char list * bool
    | FunctionName of char list
    | Nothing

  type tokenizer = {
    tokens : token list;
    text : char list;
    collecting : collecting;
  }

  let display (token : token) =
    match token with
    | Number n -> "Number(" ^ String.of_seq (List.to_seq n) ^ ")"
    | Operator op -> display_op op
    | LBrace -> "("
    | RBrace -> ")"
    | Ident f -> String.of_seq (List.to_seq f)

  let debug tokens =
    List.fold_left (fun text tok -> text ^ tok) "" (List.map display tokens)

  let init text =
    {
      tokens = [];
      text = List.of_seq (String.to_seq text);
      collecting = Nothing;
    }

  let peek parser ~value =
    match parser.text with
    | matched :: _ when matched = value -> true
    | _ -> false

  let skip text =
    let rec skip' text len =
      match len with
      | value when value > 0 -> (
          match text with [] -> text | _ :: rest -> skip' rest (value - 1))
      | _ -> text
    in

    skip' text 1

  let finalize collecting =
    match collecting with
    | Numbers (n, _) -> Some (Number (List.rev n))
    | Nothing -> None
    | FunctionName f -> Some (Ident (List.rev f))

  let rec collect parser enum =
    match (parser.collecting, enum) with
    | Numbers (cl, d), `Number char ->
        Ok
          {
            parser with
            collecting = Numbers (char :: cl, d);
            text = skip parser.text;
          }
    | Numbers (cl, false), `Dot ->
        Ok
          {
            parser with
            collecting = Numbers ('.' :: cl, true);
            text = skip parser.text;
          }
    | Numbers (_, true), `Dot -> Error `Double_dot_in_number
    | Nothing, `Number char ->
        Ok
          {
            parser with
            collecting = Numbers ([ char ], false);
            text = skip parser.text;
          }
    | Nothing, `Space -> Ok { parser with text = skip parser.text }
    | Nothing, `Token t ->
        Ok
          {
            tokens = t :: parser.tokens;
            text = skip parser.text;
            collecting = Nothing;
          }
    | collecting, other ->
        let tok = finalize collecting in
        let finalized_parser =
          match tok with
          | Some t ->
              { parser with tokens = t :: parser.tokens; collecting = Nothing }
          | None -> parser
        in
        collect finalized_parser other

  let tokenize text =
    let rec tokenize' parser =
      match parser.text with
      | [] -> (
          let tok = finalize parser.collecting in
          match tok with
          | Some t -> Ok (List.rev (t :: parser.tokens))
          | None -> Ok (List.rev parser.tokens))
      | char :: _ -> (
          match char with
          | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
              collect parser (`Number char) >>= tokenize'
          | '+' -> collect parser (`Token (Operator Plus)) >>= tokenize'
          | '-' -> collect parser (`Token (Operator Minus)) >>= tokenize'
          | '/' -> collect parser (`Token (Operator Divide)) >>= tokenize'
          | '*' -> collect parser (`Token (Operator Multiply)) >>= tokenize'
          | '(' -> collect parser (`Token LBrace) >>= tokenize'
          | ')' -> collect parser (`Token RBrace) >>= tokenize'
          | '.' -> collect parser `Dot >>= tokenize'
          | ' ' | '\t' -> collect parser `Space >>= tokenize'
          | c -> Error (`Character_not_supp c))
    in

    tokenize' @@ init text
end

module AST = struct
  open Common

  type node = Number of number | Expression of node * operator * node
  type progress = Empty | Left of node | Middle of node * operator

  type parser = {
    tokens : Token.token list;
    progress : progress;
    result : node option;
  }

  let operator a op b =
    let operator_int a op b =
      match op with
      | Plus -> Int (a + b)
      | Minus -> Int (a - b)
      | Multiply -> Int (a * b)
      | Divide -> Int (a / b)
    in

    let operator_float a op b =
      match op with
      | Plus -> Float (a +. b)
      | Minus -> Float (a -. b)
      | Multiply -> Float (a *. b)
      | Divide -> Float (a /. b)
    in

    match (a, b) with
    | Int a, Int b -> operator_int a op b
    | Float a, Float b -> operator_float a op b
    | Int a, Float b -> operator_float (float_of_int a) op b
    | Float a, Int b -> operator_float a op (float_of_int b)

  let init tokens = { tokens; progress = Empty; result = None }

  let rec display_node n =
    match n with
    | Number n -> display_num n
    | Expression (l, op, r) ->
        display_node l ^ " " ^ display_op op ^ " " ^ display_node r

  let char_list_to_num c =
    let str = String.of_seq @@ List.to_seq c in
    match String.index_opt str '.' with
    | Some _ -> Float (float_of_string str)
    | None -> Int (int_of_string str)

  let parse text =
    let rec parse' (parser : parser) =
      let first_as_num tokens num =
        { parser with tokens; progress = Left (Number num) }
      in

      let last_as_num tokens a op b =
        {
          tokens;
          progress = Empty;
          result = Some (Expression (a, op, Number b));
        }
      in

      let middle_as_op tokens l op =
        { tokens; progress = Middle (l, op); result = None }
      in

      let middle_as_op_higher tokens old_n new_op =
        match old_n with
        | Number _ -> middle_as_op tokens old_n new_op |> parse'
        | Expression (old_l, old_op, old_r) -> (
            let add_node next_n = Expression (old_l, old_op, next_n) in
            match operator_bigger new_op old_op with
            | true ->
                parse'
                  { tokens; result = None; progress = Middle (old_r, new_op) }
                |> Result.map add_node
            | false -> middle_as_op tokens old_n new_op |> parse')
      in

      match parser with
      | { tokens = []; progress = Empty; result = Some n; _ } -> Ok n
      | { tokens = Number n :: rest; progress = Empty; result = None } ->
          char_list_to_num n |> first_as_num rest |> parse'
      | { tokens = Number n :: rest; progress = Middle (l, op); result = None }
        ->
          char_list_to_num n |> last_as_num rest l op |> parse'
      | { tokens = Operator op :: rest; progress = Left l; result = None } ->
          middle_as_op rest l op |> parse'
      | { tokens = Operator op :: rest; progress = Empty; result = Some n } ->
          middle_as_op_higher rest n op
      | _ -> Error `Not_impl
    in

    parse' @@ init text

  let rec exec head =
    match head with
    | Number n -> n
    | Expression (a, op, b) -> operator (exec a) op (exec b)
end

let exec text =
  let* tokens = Token.tokenize text in
  let* nodes = AST.parse tokens in
  let result = AST.exec nodes in
  Ok result
