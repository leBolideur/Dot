open Lexer

type env_entry = { name : string; freezed : bool; value : int; history : int list }

type ast =
  | Int of int
  | Var of string
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

type statement = Decl_st of string * ast |  Decl_st_freeze of string * ast | Expr_st of ast
type program = { statements : statement list }

let rec parse_primary tokens =
  match tokens with
  | { kind = Int nb } :: tl -> (Int nb, tl)
  | { kind = LPAREN } :: tl -> (
      let right, rest = parse_add tl in
      match rest with
      | { kind = RPAREN } :: tl' -> (right, tl')
      | _ -> failwith "Missing RPAREN")
  | { kind = VAR name } :: tl ->
      (* Printf.printf "parse_primary > VAR name = %s\n" name; *)
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
  | [] -> { statements = stmts }
  | { kind = EOF } :: [] ->
      (* print_endline "\nEnd of parse"; *)
      { statements = stmts }
  | { kind = NEWLINE } :: tl -> parse tl stmts

  | { kind = VAR name } :: { kind = ASSIGN } :: tl ->
      (* Printf.printf "DECL VAR, name = %s\n" name; *)
      let node, rest = parse_expression tl in

      (match rest with
      | { kind = DOT } :: tl ->
        let stmt = Decl_st_freeze (name, node) in
        parse tl (stmt :: stmts)
      | _ ->
        let stmt = Decl_st (name, node) in
        parse rest (stmt :: stmts))

   | { kind = VAR name } :: { kind = DOT } :: tl ->
      Printf.printf "IDENT FREEZE, name = %s\n" name;
      let stmt = Expr_st (Var name) in
      parse tl (stmt :: stmts)
  | { kind = VAR name } :: tl ->
      Printf.printf "IDENT VAR, name = %s\n" name;
      let stmt = Expr_st (Var name) in
      parse tl (stmt :: stmts)
 
  | { kind = Int _ } :: _ | { kind = LPAREN } :: _ ->
      let node, rest = parse_expression tokens in
      let stmt = Expr_st node in
      parse rest (stmt :: stmts)

  | _ :: tl ->
      print_endline "Unknown";
      parse tl stmts

let rec find_in_env var_name env =
  match env with
  | [] -> None
  | entry :: _ when entry.name = var_name -> Some entry
  | _ :: tl -> find_in_env var_name tl

let rec print_env env =
  match env with
  | [] -> print_endline "End of env"
  | { name = var_name; freezed = _; value = var_value; history = _ } :: tl ->
      Printf.printf "VAR %s = %d\n" var_name var_value;
      print_env tl

let rec eval_ast ast env =
  match ast with
  | Int nb -> nb
  | Var name -> (
      match find_in_env name env with
      | None -> failwith "Unbound variable"
      | Some entry -> entry.value)
  | Add (left, right) -> eval_ast left env + eval_ast right env
  | Minus (left, right) -> eval_ast left env - eval_ast right env
  | Mul (left, right) -> eval_ast left env * eval_ast right env
  | Div (left, right) -> eval_ast left env / eval_ast right env

let update_variable ast var = 
  match var.freezed with
  | true -> failwith "Freezed variable!"
  | false ->
    let new_value = ast in
    let new_history = List.rev (new_value :: var.history) in
    let new_var = {
      name = var.name;
      freezed = false;
      value = new_value;
      history = new_history;
    } in
    Printf.printf "Update var for %s >>> %d\n" var.name new_var.value;
    new_var

let rec print_var_history var index = 
  match var.history with
  | [] ->  print_endline "No history"
  | _ :: _ when (index >= List.length var.history) -> print_endline "End history"
  | _ :: _ -> 
    Printf.printf "%s@%d = %d\n" var.name index (List.nth var.history index);
    print_var_history var (index + 1)

let rec pop_env_by_var_name env name = 
  match env with
  | [] -> []
  | hd :: tl when hd.name == name -> tl
  | _ :: tl -> pop_env_by_var_name tl name

let rec eval program_stmts env =
  match program_stmts with
  | [] -> env
  | Decl_st (var_name, ast) :: tl ->
      let new_env =
        match find_in_env var_name env with
        | None ->
            Printf.printf "%s not existing in env\n" var_name;
            let value = eval_ast ast env in
            let var =
              { name = var_name; freezed = false; value = value; history = value :: [] }
            in
            (* Printf.printf "Eval Decl_st for %s >>> %d\n" var_name var.value; *)
            (* print_env env; *)
            var :: env
        | Some entry ->
            Printf.printf "%s Exist with value: %d\n" entry.name entry.value;
            let updated_var = update_variable (eval_ast ast env) entry in
            print_endline "\nHistory:";
            print_var_history updated_var 0;
            (* print_env env; *)
            let new_env = pop_env_by_var_name env updated_var.name in
            updated_var :: new_env
      in
      eval tl new_env
  | Decl_st_freeze (var_name, ast) :: tl ->
      let new_env =
        match find_in_env var_name env with
        | None ->
            Printf.printf "freeze - %s not existing in env\n" var_name;
            let value = eval_ast ast env in
            let var =
              { name = var_name; freezed = true; value = value; history = value :: [] }
            in
            var :: env
        | Some entry ->
            Printf.printf "freeze - %s Exist with value: %d\n" entry.name entry.value;
            let updated_var = update_variable (eval_ast ast env) entry in
            let freezed_var = { updated_var with freezed = true } in
            print_endline "\nHistory:\nFREEZED DECL.!";
            print_var_history freezed_var 0;
            (* print_env env; *)
            let new_env = pop_env_by_var_name env freezed_var.name in
            freezed_var :: new_env
      in
      eval tl new_env
  | Expr_st ast :: tl ->
      Printf.printf "Eval Expr_st >>> %d\n" (eval_ast ast env);
      eval tl env
