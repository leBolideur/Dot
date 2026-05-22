open Parser
open Env

let rec eval_ast ast env =
  match ast with
  | IntLit nb -> VInt nb
  | StrLit str -> VStr str
  | Ident name -> VIdent name
  | Var name -> (
      match find_in_env env name with
      | None ->
          Printf.printf "Unbound variable %s\n" name;
          failwith ""
      | Some entry -> entry.value)
  | Add (left, right) -> (
      let x = eval_ast left env in
      let y = eval_ast right env in
      match (x, y) with
      | VInt a, VInt b -> VInt (a + b)
      | _ -> failwith "Type mismatch for operator +")
  | Minus (left, right) -> (
      let x = eval_ast left env in
      let y = eval_ast right env in
      match (x, y) with
      | VInt a, VInt b -> VInt (a - b)
      | _ -> failwith "Type mismatch for operator -")
  | Mul (left, right) -> (
      let x = eval_ast left env in
      let y = eval_ast right env in
      match (x, y) with
      | VInt a, VInt b -> VInt (a * b)
      | _ -> failwith "Type mismatch for operator *")
  | Div (left, right) -> (
      let x = eval_ast left env in
      let y = eval_ast right env in
      match (x, y) with
      | VInt a, VInt b -> VInt (a / b)
      | _ -> failwith "Type mismatch for operator /")
  | Eq (left, right) -> (
      let x = eval_ast left env in
      let y = eval_ast right env in
      match (x, y) with
      | VInt a, VInt b -> VBool (a == b)
      | _ -> failwith "Type mismatch for operator ==")


let rec print_stmts stmts = 
    match stmts with
    | [] -> print_endline "End of stmts list"
    | Decl_st (name, _, freezed) :: tl ->
        Printf.printf "Decl_st -> name = %s\tfreezed = %b\n" name freezed;
        print_stmts tl
    | Expr_Builtin_st (name, ast) :: tl ->
        Printf.printf "Expr_Builtin_st -> name = %s\n\tast:\n\t" name;
        print_ast ast;
        print_stmts tl
    | _ :: tl -> print_endline "Other stmt"; print_stmts tl

let rec run program_stmts env =
  match program_stmts with
  | [] -> env
  | Decl_st (var_name, ast, is_freezed) :: tl ->
      let new_env =
        match find_in_env env var_name with
        | None ->
            let value = eval_ast ast env in
            let var =
              {
                name = var_name;
                freezed = is_freezed;
                value;
                history = value :: [];
              }
            in
            var :: env
        | Some _ when is_freezed -> failwith "Variable already freezed"
        | Some entry ->
            let updated_var = update_variable (eval_ast ast env) entry in
            let new_env = pop_env_by_var_name env updated_var.name in
            updated_var :: new_env
      in
      run tl new_env
  | Freeze_st name :: tl ->
      let var_to_freeze =
        match find_in_env env name with
        | Some entry -> entry
        | None -> failwith ("Unable to freeze this var: " ^ name)
      in
      let new_var = { var_to_freeze with freezed = true } in
      let new_env = pop_env_by_var_name env name in

      run tl (new_var :: new_env)
  | Expr_Builtin_st (name, ast) :: tl -> (
      let node = eval_ast ast env in
      match (name, node) with
      | "print", VInt nb ->
          Printf.printf "%d\n" nb;
          run tl env
      | "print", VStr str ->
          Printf.printf "%s\n" str;
          run tl env
      | "print", VBool boolean ->
          Printf.printf "%b\n" boolean;
          run tl env
      | _ -> failwith "Unknown builtin")
  | Stmt_Builtin_st (name, ident) :: tl -> (
      match (name, ident) with
      | "debug", var_name -> (
          let history = var_history_by_name env var_name in
          match history with
          | None ->
              print_env env;
              Printf.printf ".debug > no history for %s\n" var_name;
              run tl env
          | Some hist ->
              Printf.printf ".debug > history len for %s : %d\n" var_name (List.length hist);
              print_history var_name hist 0;
              Printf.printf "Is freezed? %b\n" (is_var_name_freezed env var_name);
              run tl env)
      | _ -> failwith "Unknown builtin")
  | Expr_st _ :: tl -> run tl env
  | If_st (cond, consequence, alternative) :: tl ->
    let eval_cond = eval_ast cond env in
    match eval_cond with
    | VBool true ->
        let new_env = run (List.rev consequence) env in
        run tl new_env
    | _ -> 
        let new_env = run (List.rev alternative) env in
        run tl new_env
   
