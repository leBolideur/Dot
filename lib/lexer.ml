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
  | RIGHT_ARROW
  | COMMA

type token = { kind : token_type }
type state = { index : int; line : int }

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
  | Some _ | None -> (state, String.sub input start (state.index - start))

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
  | Some '.' when is_next_builtin input state -> (
      (* Consume the dot *)
      let advanced_state = advance state in
      let state, builtin_name =
        read_builtin_name input advanced_state advanced_state.index
      in
      match if_builtin_get_token builtin_name with
      | Some token -> lex input (advance state) (token :: tokens)
      | None -> failwith "Unknown builtin!")
  | Some '.' ->
      let token = { kind = DOT } in
      lex input (advance state) (token :: tokens)
  | Some ',' ->
      let token = { kind = COMMA } in
      lex input (advance state) (token :: tokens)
  | Some 'a' .. 'z' -> (
      let state, result = read_ident input state state.index in
      match if_keyword_get_token result with
      | Some token -> lex input state (token :: tokens)
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
  | Some '-' when is_next_char input state '>' ->
      let token = { kind = RIGHT_ARROW } in
      lex input (advance (advance state)) (token :: tokens)
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
      List.rev (token :: tokens)
  | _ -> lex input (advance state) tokens
