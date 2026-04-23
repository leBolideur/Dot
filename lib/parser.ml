open Lexer

type ast =
  | Int of int
  | Var of string
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

type statement = Decl_st of string * ast | Expr_st of ast
type program = Program of statement list

let rec parse_primary tokens =
  match tokens with
  | { kind = Int nb } :: tl -> (Int nb, tl)
  | { kind = LPAREN } :: tl -> (
      let right, rest = parse_add tl in
      match rest with
      | { kind = RPAREN } :: tl' -> (right, tl')
      | _ -> failwith "Missing RPAREN")
  | { kind = VAR name } :: tl ->
      Printf.printf "parse_primary > VAR name = %s\n" name;
      (Var name, tl)
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
  | Int nb -> Printf.printf "%d" nb
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

let rec parse tokens stmts =
  match tokens with
  | [] -> Program stmts
  | { kind = EOF } :: [] ->
      print_endline "\nEnd of parse";
      Program stmts
  | { kind = NEWLINE } :: tl -> parse tl stmts
  | { kind = VAR name } :: { kind = ASSIGN } :: tl ->
      Printf.printf "DECL VAR, name = %s\n" name;
      let node, rest = parse_var_st name tl in
      let stmt = Decl_st (name, node) in
      parse rest (stmt :: stmts)
  | { kind = VAR name } :: tl ->
      Printf.printf "IDENT VAR, name = %s\n" name;
      let stmt = Expr_st (Var name) in
      parse tl (stmt :: stmts)
  | { kind = Int _ } :: _ | { kind = LPAREN } :: _ ->
      let node, rest = parse_expression tokens in
      let stmt = Expr_st node in
      parse rest (stmt :: stmts)
  | _ :: tl -> print_endline "Unknown"; parse tl stmts

let rec eval_ast ast =
  match ast with
  | Int nb -> nb
  | Var name -> Printf.printf "VAR %s = %d\n" name (eval_ast ast); 66
  | Add (left, right) -> eval_ast left + eval_ast right
  | Minus (left, right) -> eval_ast left - eval_ast right
  | Mul (left, right) -> eval_ast left * eval_ast right
  | Div (left, right) -> eval_ast left / eval_ast right

let rec eval program_stmts env =
  match program_stmts with
  | [] -> env
  | Decl_st (name, ast) :: tl ->
      Printf.printf "Eval Decl_st for %s >>> %d\n" name (eval_ast ast);
      eval tl env
  | Expr_st ast :: tl ->
      Printf.printf "Eval Expr_st >>> %d\n" (eval_ast ast);
      eval tl env
