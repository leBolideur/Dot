open Lexer

type ast =
  | Int of int
  | Var of string * int
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

type statement = Decl_st of string * statement | Expr_st of ast
type program = statement list

let rec parse_primary tokens =
  match tokens with
  | { kind = Int nb } :: tl -> (Int nb, tl)
  | { kind = LPAREN } :: tl -> (
      let right, rest = parse_add tl in
      match rest with
      | { kind = RPAREN } :: tl' -> (right, tl')
      | _ -> failwith "Missing RPAREN")
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
  | Var (name,_) ->
      Printf.printf "VAR %s = " name;
      (* print_ast expr *)
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
  let expr = Expr_st ast in
  (* let right, rest = parse_expression tokens in *)
  (expr, rest)

let parse_var_st name tokens =
  let expr, rest = parse_expression tokens in
  let stmt = Decl_st (name, expr) in
  (stmt, rest)

let rec parse tokens stmts =
  match tokens with
  | [] -> stmts
  | { kind = EOF } :: [] ->
      print_endline "\nEnd of parse";
      stmts
  | { kind = VAR name } :: { kind = EQ } :: tl ->
      let node, rest = parse_var_st name tl in
      let stmt = Decl_st (name, node) in
      parse rest (stmt :: stmts)
  | { kind = Int _ } :: _ | { kind = LPAREN } :: _ ->
      let node, rest = parse_expression tokens in
      (* let stmt = Expr_st node in *)
      parse rest (node :: stmts)
  | _ :: tl -> parse tl stmts

let rec eval_ast ast =
  match ast with
  | Int nb -> nb
  | Var (_, value) -> value
  | Add (left, right) -> eval_ast left + eval_ast right
  | Minus (left, right) -> eval_ast left - eval_ast right
  | Mul (left, right) -> eval_ast left * eval_ast right
  | Div (left, right) -> eval_ast left / eval_ast right

let rec eval program env =
  match program with
  | [] -> env
  | Decl_st (_, _) :: tl -> eval tl env
  | Expr_st ast :: tl ->
      Printf.printf "Eval >>> %d\n" (eval_ast ast);
      eval tl env
