type token_type =
  | INT of int
  | IDENT of string
  | VAR of string
  | STR_LIT of string
  | EXPR_BUILTIN of string
  | STMT_BUILTIN of string
  | DOT
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
  | IF
  | ELSE

type token = { kind : token_type }
type state = { index : int; line : int }

let print_token token =
  match token.kind with
  | INT nb -> Printf.printf "INT : %d\n" nb
  | IDENT ident -> Printf.printf "IDENT : %s\n" ident
  | VAR name -> Printf.printf "VAR : %s\n" name
  | STR_LIT str -> Printf.printf "STR_LIT : %s\n" str
  | EXPR_BUILTIN name -> Printf.printf "EXPR_BUILTIN : %s\n" name
  | STMT_BUILTIN name -> Printf.printf "STMT_BUILTIN : %s\n" name
  | DOT -> print_endline "DOT ."
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
  | IF -> print_endline "IF"
  | ELSE -> print_endline "ELSE"
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

let rec read_builtin_name input state start = 
  match peek input state with
  | Some 'a' .. 'z' -> read_builtin_name input (advance state) start
  | Some ' ' | Some _ | None ->
    let name =  String.sub input start (state.index - start) in
    Printf.printf "\nread_bi > name = %s\tindex: %d - start: %d\n" name state.index start;
      (state, name)

let is_next_builtin input state =
  match peek input (advance state) with Some 'a' .. 'z' -> true | _ -> false

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
      let number = int_of_string sub in
      (state, number)

let is_next_char input state expected =
  match peek input (advance state) with Some c -> expected == c | _ -> false

let if_keyword_get_token str =
  match str with
  | "if" -> Some { kind = IF }
  | "else" -> Some { kind = ELSE }
  | _ -> None

let if_builtin_get_token str =
  match str with
  | "print" -> Some { kind = EXPR_BUILTIN str }
  | "debug" -> Some { kind = STMT_BUILTIN str }
  | _ -> None

let rec lex input state tokens =
  match peek input state with
  | Some ' ' -> lex input (advance state) tokens
  | Some '\n' ->
      let token = { kind = NEWLINE } in
      lex input
        { index = state.index + 1; line = state.line + 1 }
        (token :: tokens)
  | Some '.' when is_next_builtin input state ->
      let state, builtin_name = read_builtin_name input state state.index in
      Printf.printf "\nbuiltin_name = %s\n" builtin_name;
      (match if_builtin_get_token builtin_name with
        | Some token -> lex input (advance state) (token :: tokens)
        | None -> failwith "Unknown builtin!")
  | Some '.' ->
      let token = { kind = DOT } in
      lex input (advance state) (token :: tokens)
  | Some 'a' .. 'z' -> (
      let state, result = read_ident input state state.index in
      match if_keyword_get_token result with
      | Some token ->
          lex input state (token :: tokens)
      | None -> (
          match if_builtin_get_token result with
          | Some token -> lex input state (token :: tokens)
          | _ ->
            let token = { kind = IDENT result } in
            lex input state (token :: tokens)))
  | Some 'A' .. 'Z' ->
      let state, result = read_ident input state state.index in
      let token = { kind = VAR result } in
      lex input state (token :: tokens)
  | Some '0' .. '9' ->
      let state, int = read_int input state state.index in
      let token = { kind = INT int } in
      lex input state (token :: tokens)
  | Some '"' ->
      (* Consume the quote *)
      let advanced_state = advance state in
      let state, result =
        read_string_literal input advanced_state advanced_state.index
      in
      let token = { kind = STR_LIT result } in
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
