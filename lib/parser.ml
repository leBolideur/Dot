open Lexer

type ast =
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
  | Add (left, right) -> eval left + eval right
  | Minus (left, right) -> eval left - eval right
  | Mul (left, right) -> eval left * eval right
  | Div (left, right) -> eval left / eval right

let parse_expr tokens =
  (* let tokens = *)
  (* [Lexer.LPAREN; Lexer.Int 2; Lexer.PLUS; Lexer.Int 3; Lexer.RPAREN; Lexer.STAR; Lexer.Int 4 ] *)
  (* in *)
  let ast, _ = parse_add tokens in
  print_ast ast;
  (* Printf.printf " Result >>> %d\n" (eval ast); *)
  (* print_endline "Parsing done!" *)
  eval ast
