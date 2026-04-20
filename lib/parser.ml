open Lexer

type ast =
  | Int of int
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

let rec  parse_primary tokens =
  match tokens with
  | Lexer.Int nb :: tl -> (Int nb, tl)
  | Lexer.LPAREN :: tl -> 
      let ast, result = parse_add tl in
      (match result with
      | Lexer.RPAREN :: tl -> (ast, tl)
      | _ -> failwith "Missing RPAREN")
  | _ -> failwith "Fail parse_primary"

and parse_factor tokens =
  let left, rest = parse_primary tokens in
  match rest with
  (* | [] -> () *)
  | STAR :: tl ->
      let right, rest = parse_primary tl in
      (Mul (left, right), rest)
  | DIV :: tl ->
      let right, rest = parse_primary tl in
      (Div (left, right), rest)
  | _ -> (left, rest)

and parse_add tokens =
  let left, rest = parse_factor tokens in
  match rest with
  (* | [] -> () *)
  | PLUS :: tl ->
      let right, rest = parse_add tl in
      (Add (left, right), rest)
  | MINUS :: tl ->
      let right, rest = parse_add tl in
      (Minus (left, right), rest)
  | _ -> (left, rest)

let rec print_ast ast =
  match ast with
  | Int nb -> Printf.printf "%d" nb
  | Add (left, right) ->
      print_ast left;
      Printf.printf " + ";
      print_ast right
  | Minus (left, right) ->
      print_ast left;
      Printf.printf " - ";
      print_ast right
  | Mul (left, right) ->
      print_ast left;
      Printf.printf " * ";
      print_ast right
  | Div (left, right) ->
      print_ast left;
      Printf.printf " / ";
      print_ast right

let rec eval ast =
  match ast with
  | Int nb -> nb
  | Add (left, right) -> eval left + eval right
  | Minus (left, right) -> eval left - eval right
  | Mul (left, right) -> eval left * eval right
  | Div (left, right) -> eval left / eval right

let parse_expr =
  let tokens =
    [Lexer.LPAREN; Lexer.Int 2; Lexer.PLUS; Lexer.Int 3; Lexer.RPAREN; Lexer.STAR; Lexer.Int 4 ]
  in
  let ast, _ = parse_add tokens in
  print_ast ast;
  Printf.printf " Result >>> %d\n" (eval ast);
  print_endline "Parsing done!"
