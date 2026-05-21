open Lexer

type value = VInt of int | VStr of string | VIdent of string | VBool of bool | VUnit

type ast =
  | IntLit of int
  | StrLit of string
  | Ident of string
  | Var of string
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Eq of ast * ast

type statement =
  | Decl_st of string * ast * bool (* bool = to_freeze *)
  | Expr_st of ast
  | Freeze_st of string
  | Expr_Builtin_st of string * ast
  | Stmt_Builtin_st of string * string
  | If_st of ast * statement list * statement list

type program = { statements : statement list }

let rec parse_primary tokens =
  match tokens with
  | [] -> failwith "Fail parse_primary: empty tokens list"
  | { kind = INT nb } :: tl -> (IntLit nb, tl)
  | { kind = STR_LIT str } :: tl -> (StrLit str, tl)
  | { kind = IDENT name } :: tl -> (Ident name, tl)
  | { kind = VAR name } :: tl -> (Var name, tl)
  | { kind = LPAREN } :: tl -> (
      let right, rest = parse_add tl in
      match rest with
      | { kind = RPAREN } :: tl' -> (right, tl')
      | _ -> failwith "Missing RPAREN")
  | token :: _ ->
     print_token token;
     failwith "Fail parse_primary"
  

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
  | Ident name -> Printf.printf "Ident: %s" name
  | Var name -> Printf.printf "Var %s" name
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

let parse_comparison tokens = 
  let left, rest = parse_add tokens in
  match rest with
  | { kind = EQ } :: tl -> 
    let right, rest' = parse_add tl in
    (Eq (left, right), rest')
  | _ ->
    (left, rest)

let parse_expression tokens =
  let ast, rest = parse_comparison tokens in
  (ast, rest)

let parse_var_st name tokens =
  let _, rest = parse_expression tokens in
  (Var name, rest)

let check_freeze tokens =
  match tokens with
  | { kind = DOT } :: tl -> (true, tl)
  | { kind = NEWLINE } :: tl -> (false, tl)
  | _ -> failwith "Expected . or newline"

let rec parse_block_statements tokens =
  match tokens with 
  | { kind = DOT } :: tl -> ([], tl)
  | rest ->
    let ({ statements = stmts }, rest) = parse rest [] in
    (stmts, rest)

and parse tokens stmts =
  match tokens with
  | [] -> ({ statements = stmts }, [])
  | { kind = EOF } :: _ -> ({ statements = stmts }, [])
  | { kind = NEWLINE } :: tl -> parse tl stmts
  | { kind = DOT } :: { kind = EXPR_BUILTIN name } :: tl ->
      let node, rest = parse_expression tl in
      let stmt = Expr_Builtin_st (name, node) in
      parse rest (stmt :: stmts)
  | { kind = DOT }
    :: { kind = STMT_BUILTIN name }
    :: { kind = VAR ident }
    :: tl ->
      let stmt = Stmt_Builtin_st (name, ident) in
      parse tl (stmt :: stmts)
  | { kind = DOT } :: tl ->
    (* End of block*)
    ({ statements = stmts}, tl)
  | { kind = VAR name } :: { kind = ASSIGN } :: tl ->
      let node, rest = parse_expression tl in
      let to_freeze, rest' = check_freeze rest in
      let stmt = Decl_st (name, node, to_freeze) in
      parse rest' (stmt :: stmts)
  | { kind = VAR name } :: { kind = DOT } :: tl ->
      let stmt = Freeze_st name in
      parse tl (stmt :: stmts)
  | { kind = VAR name } :: tl ->
      let stmt = Expr_st (Ident name) in
      parse tl (stmt :: stmts)
  | { kind = INT _ } :: _ | { kind = LPAREN } :: _ ->
      let node, rest = parse_expression tokens in
      let stmt = Expr_st node in
      parse rest (stmt :: stmts)
  | { kind = IF } :: tl -> (
      let condition, rest = parse_expression tl in

      match rest with
      | { kind = NEWLINE } :: tl -> 
        let (if_stmts, rest') = parse_block_statements tl in
        print_token (List.nth rest' 0);

        (match rest' with
        | { kind = ELSE } :: tl' -> 
          let (else_stmts, rest) = parse_block_statements tl' in
          Printf.printf "parser - else stmts len > %d\n" (List.length else_stmts);
          let stmt = If_st (condition, if_stmts, else_stmts) in
          parse rest (stmt :: stmts)
          (* | _ -> failwith "Syntaxe error, newline expected")*)
        | _ ->
          let stmt = If_st (condition, if_stmts, []) in
          parse rest (stmt :: stmts))

      | _ -> failwith "Syntaxe error, newline expected")
  | token :: tl ->
      Printf.printf "Unknown token ";
      print_token token;
      parse tl stmts
