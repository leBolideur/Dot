open Lexer

type ast =
  | Var of string * ast
  | Int of int
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

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
  | Var (name, expr) -> Printf.printf "VAR %s = " name; print_ast expr
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

let rec eval ast =
  match ast with
  | Int nb -> nb
  | Var (_, expr) -> eval expr
  | Add (left, right) -> eval left + eval right
  | Minus (left, right) -> eval left - eval right
  | Mul (left, right) -> eval left * eval right
  | Div (left, right) -> eval left / eval right

let parse_expression tokens =
  (* let tokens = *)
  (* [Lexer.LPAREN; Lexer.Int 2; Lexer.PLUS; Lexer.Int 3; Lexer.RPAREN; Lexer.STAR; Lexer.Int 4 ] *)
  (* in *)
  let ast, _ = parse_add tokens in
  print_ast ast;
  (* Printf.printf " Result >>> %d\n" (eval ast); *)
  (* print_endline "Parsing done!" *)
  (* eval ast *)
  (ast, List.tl tokens)

let parse_var_st name tokens = 
  let right, rest = parse_expression tokens in
  (Var (name, right), rest)


let parse_statement tokens = 
  match tokens with
  | [] -> []
  | { kind = VAR name } :: { kind = EQ } :: tl -> 
    parse_var_st name tl
  | _ -> failwith "Unknown statement"

let rec parse tokens stmts = 
  Printf.printf "BF >> tokens length = %d\n" (List.length tokens);

  match tokens with
  | { kind = EOF } ->
    print_endline "End of parse";
    Printf.printf "AF >> acc length = %d ------ rest length = %d\n" (List.length stmts) (List.length rest)
  | _ ->
    let stmt, rest = parse_statement tokens [] in
    parse rest (stmt :: stmts)
