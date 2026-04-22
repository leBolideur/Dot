type token_type =
  | Int of int
  | Ident of string
  | VAR of string
  | Str_lit of string
  | Dot
  | EOF
  | ASSIGN
  | EQ
  | GT
  | GTE
  | LT
  | LTE
  | BANG
  | STAR
  | PLUS
  | MINUS
  | DIV
  | LPAREN
  | RPAREN
  | NEWLINE
  | DIFF

type token = { kind : token_type }
type state = { index : int; line : int }

let print_token token =
  match token.kind with
  | Int nb -> Printf.printf "INT : %d\n" nb
  | Ident ident -> Printf.printf "IDENT : %s\n" ident
  | VAR name -> Printf.printf "VAR : %s\n" name
  | Str_lit str -> Printf.printf "STRING : %s\n" str
  | Dot -> print_endline "DOT ."
  | ASSIGN -> print_endline "ASSIGN"
  | EQ -> print_endline "EQ"
  | GT -> print_endline "GT"
  | GTE -> print_endline "GTE"
  | LT -> print_endline "LT"
  | LTE -> print_endline "LTE"
  | BANG -> print_endline "BANG"
  | STAR -> print_endline "STAR"
  | PLUS -> print_endline "PLUS"
  | MINUS -> print_endline "MINUS"
  | DIV -> print_endline "DIV"
  | LPAREN -> print_endline "LPAREN"
  | RPAREN -> print_endline "RPAREN"
  | DIFF -> print_endline "DIFF"
  | NEWLINE -> print_endline "NEWLINE"
  | EOF -> print_endline "EOF - So far so good!"

let peek input state =
  if state.index >= String.length input then None else Some input.[state.index]

let advance state = { state with index = state.index + 1 }

let rec read_ident input state start =
  match peek input state with
  | Some 'a' .. 'z' | Some 'A' .. 'Z' | Some '0' .. '9' | Some '_' ->
      read_ident input (advance state) start
  | Some ' ' | Some _ | None ->
      (state, String.sub input start (state.index - start))

let rec read_string_literal input state start =
  match peek input state with
  | Some '"' -> (state, String.sub input start (state.index - start))
  | Some _ -> read_string_literal input (advance state) start
  | None -> (state, "Error: String lit. error")

let rec read_int input state start =
  match peek input state with
  | Some '0' .. '9' -> read_int input (advance state) start
  | Some _ | None ->
      let sub = String.sub input start (state.index - start) in
      let int = int_of_string sub in
      (state, int)

let is_next_char input state expected =
  match peek input (advance state) with Some c -> expected == c | _ -> false

let rec lex input state tokens =
  match peek input state with
  | Some ' ' -> lex input (advance state) tokens
  | Some '\n' ->
    let token = { kind = NEWLINE} in
      lex input { index = state.index + 1; line = state.line + 1 } (token :: tokens)
  | Some '.' ->
      let token = { kind = Dot } in
      lex input (advance state) (token :: tokens)
  | Some 'a' .. 'z' ->
      let state, result = read_ident input state state.index in
      let token = { kind = Ident result } in
      lex input state (token :: tokens)
  | Some 'A' .. 'Z' ->
      let state, result = read_ident input state state.index in
      let token = { kind = VAR result } in
      lex input state (token :: tokens)
  | Some '0' .. '9' ->
      let state, int = read_int input state state.index in
      let token = { kind = Int int } in
      lex input state (token :: tokens)
  | Some '"' ->
      (* Consume the quote *)
      let advanced_state = advance state in
      let state, result =
        read_string_literal input advanced_state advanced_state.index
      in
      let token = { kind = Str_lit result } in
      lex input (advance state) (token :: tokens)
  | Some '=' when is_next_char input state '=' ->
      let token = { kind = EQ } in
      lex input (advance (advance state)) (token :: tokens)
  | Some '=' ->
      let token = { kind = ASSIGN } in
      lex input (advance state) (token :: tokens)
  | Some '>' when is_next_char input state '=' ->
      let token = { kind = GTE } in
      lex input (advance (advance state)) (token :: tokens)
  | Some '>' ->
      let token = { kind = GT } in
      lex input (advance state) (token :: tokens)
  | Some '<' when is_next_char input state '=' ->
      let token = { kind = LTE } in
      lex input (advance (advance state)) (token :: tokens)
  | Some '<' ->
      let token = { kind = LT } in
      lex input (advance state) (token :: tokens)
  | Some '!' when is_next_char input state '=' ->
      let token = { kind = DIFF } in
      lex input (advance (advance state)) (token :: tokens)
  | Some '!' ->
      let token = { kind = BANG } in
      lex input (advance state) (token :: tokens)
  | Some '*' ->
      let token = { kind = STAR } in
      lex input (advance state) (token :: tokens)
  | Some '+' ->
      let token = { kind = PLUS } in
      lex input (advance state) (token :: tokens)
  | Some '-' ->
      let token = { kind = MINUS } in
      lex input (advance state) (token :: tokens)
  | Some '/' ->
      let token = { kind = DIV } in
      lex input (advance state) (token :: tokens)
  | Some '(' ->
      let token = { kind = LPAREN } in
      lex input (advance state) (token :: tokens)
  | Some ')' ->
      let token = { kind = RPAREN } in
      lex input (advance state) (token :: tokens)
  | None ->
      let token = { kind = EOF } in
      token :: tokens
  | _ -> lex input (advance state) tokens
