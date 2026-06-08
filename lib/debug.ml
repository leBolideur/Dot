open Lexer

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
  | RIGHT_ARROW -> print_endline "RIGHT_ARROW"
  | COMMA -> print_endline "COMMA"
  | EOF -> print_endline "EOF"

(* let rec print_ast ast =
  match ast with
  | IntLit number ->
      let number_str = string_of_int number in
      Printf.printf "%s" number_str
  | StrLit str -> Printf.printf "StrLit %s" str
  | Ident name -> Printf.printf "Ident: %s" name
  | Var name -> Printf.printf "Var %s" name
  | Call (name, _) -> Printf.printf "Call %s" name
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
  | Eq (left, right) ->
      Printf.printf "(";
      print_ast left;
      Printf.printf " == ";
      print_ast right;
      Printf.printf ")"
*)

(*let rec print_stmts stmts =
  match stmts with
  | [] -> print_endline "\nEnd of stmts list"
  | Decl_st (name, _, freezed) :: tl ->
      Printf.printf "Decl_st -> name = %s\tfreezed = %b\n" name freezed;
      print_stmts tl
  | Expr_Builtin_st (name, ast) :: tl ->
      Printf.printf "Expr_Builtin_st -> name = %s\n\tast:\n\t" name;
      print_ast ast;
      print_stmts tl
  | Expr_st expr :: tl ->
      Printf.printf "Expr_st -> ast:\n\t";
      print_ast expr;
      print_stmts tl
  | Func_st (name, params, stmts) :: tl ->
      Printf.printf "Func_st -> name = %s\n\tparams len: %d\tstmts len: %d\n" name (List.length params) (List.length stmts);
      print_stmts tl
  | Call_st (name, args) :: tl ->
      Printf.printf "Call_st -> name = %s\n\targs len: %d\n\tast:\n\t" name (List.length args);
      List.iter print_ast args;
      print_stmts tl
  | _ :: tl ->
      print_endline "Other stmt";
      print_stmts tl
      *)

  