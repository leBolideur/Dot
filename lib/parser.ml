open Lexer

type value = VInt of int | VStr of string

type ast =
  | IntLit of int
  | StrLit of string
  | Var of string
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

type statement =
  | Decl_st of string * ast * bool
  | Expr_st of ast
  | Freeze_st of string

type program = { statements : statement list }

let rec parse_primary tokens =
  match tokens with
  | { kind = INT nb } :: tl ->
      let value = IntLit nb in
      (value, tl)
  | { kind = LPAREN } :: tl -> (
      let right, rest = parse_add tl in
      match rest with
      | { kind = RPAREN } :: tl' -> (right, tl')
      | _ -> failwith "Missing RPAREN")
  | { kind = VAR name } :: tl -> (Var name, tl)
  | _ -> failwith "Fail parse_primary"

and parse_factor tokens =
  let left, rest = parse_primary tokens in
  parse_factor_aux left rest

and parse_factor_aux left rest =
  match rest with
  | { kind = STAR } :: tl ->
      let right, rest' = parse_primary tl in
      let node = Mul (left, right) in
      parse_factor_aux node rest'
  | { kind = DIV } :: tl ->
      let right, rest' = parse_primary tl in
      let node = Div (left, right) in
      parse_factor_aux node rest'
  | _ -> (left, rest)

and parse_add tokens =
  let left, rest = parse_factor tokens in
  parse_add_aux left rest

and parse_add_aux left rest =
  match rest with
  | { kind = PLUS } :: tl ->
      let right, rest' = parse_factor tl in
      let node = Add (left, right) in
      parse_add_aux node rest'
  | { kind = MINUS } :: tl ->
      let right, rest' = parse_factor tl in
      let node = Minus (left, right) in
      parse_add_aux node rest'
  | _ -> (left, rest)

let rec print_ast ast =
  match ast with
  (* | Int nb -> Printf.printf "%d" nb *)
  | IntLit number ->
      let number_str = string_of_int number in
      Printf.printf "%s" number_str
  | StrLit str -> Printf.printf "%s" str
  | Var name -> Printf.printf "VAR %s = " name
  | Add (left, right) ->
      Printf.printf "(";
      print_ast left;
      Printf.printf " + ";
      print_ast right;
      Printf.printf ")"
  | Minus (left, right) ->
      Printf.printf "(";
      print_ast left;
      Printf.printf " - ";
      print_ast right;
      Printf.printf ")"
  | Mul (left, right) ->
      Printf.printf "(";
      print_ast left;
      Printf.printf " * ";
      print_ast right;
      Printf.printf ")"
  | Div (left, right) ->
      Printf.printf "(";
      print_ast left;
      Printf.printf " / ";
      print_ast right;
      Printf.printf ")"

let parse_expression tokens =
  let ast, rest = parse_add tokens in
  (ast, rest)

let parse_var_st name tokens =
  let _, rest = parse_expression tokens in
  (Var name, rest)

let check_freeze tokens = 
  match tokens with
    | { kind = DOT } :: tl -> (true, tl)
    | { kind = NEWLINE } :: tl -> (false, tl)
    | _ -> failwith "Expected . or newline"

let rec parse tokens stmts =
  match tokens with
  | [] -> { statements = stmts }
  | { kind = EOF } :: [] -> { statements = stmts }
  | { kind = NEWLINE } :: tl -> parse tl stmts
  | { kind = VAR name } :: { kind = ASSIGN } :: { kind = STR_LIT s } :: tl ->
    let node = StrLit s in
    let to_freeze, rest = check_freeze tl in
    let stmt = Decl_st (name, node, to_freeze) in
    parse rest (stmt :: stmts)
  | { kind = VAR name } :: { kind = ASSIGN } :: tl -> 
      let node, rest = parse_expression tl in

      let to_freeze, rest' = check_freeze rest in
      let stmt = Decl_st (name, node,  to_freeze) in
      parse rest' (stmt :: stmts)
  | { kind = VAR name } :: { kind = DOT } :: tl ->
      let stmt = Freeze_st name in
      parse tl (stmt :: stmts)
  | { kind = VAR name } :: tl ->
      let stmt = Expr_st (Var name) in
      parse tl (stmt :: stmts)
  | { kind = INT _ } :: _ | { kind = LPAREN } :: _ ->
      let node, rest = parse_expression tokens in
      let stmt = Expr_st node in
      parse rest (stmt :: stmts)
  | _ :: tl ->
      print_endline "Unknown";
      parse tl stmts
