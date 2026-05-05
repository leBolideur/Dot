open Parser
open Env

let rec var_history_by_name env name = 
    match env with
    | [] -> None
    | hd :: _ when hd.name == name -> Some hd.history
    | _ :: tl -> var_history_by_name tl name

let rec eval_ast ast env =
  match ast with
  | IntLit nb -> VInt nb
  | StrLit str -> VStr str
  | Ident name -> 
    Printf.printf "IIIIIIII VIdent for %s\n" name;
    VIdent name
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

            print_var_history updated_var 0;

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
  | Expr_Builtin_st (name, ast) :: tl -> 
    let node = eval_ast ast env in
      (match name, node with
      | "print", VInt nb -> Printf.printf "%d\n" nb; run tl env
      | "print", VStr str -> Printf.printf "%s\n" str; run tl env
      | _ -> failwith "Unknown builtin" )
  | Stmt_Builtin_st (name, ident) :: tl -> 
      (match name, ident with
      | "debug", var_name -> (
        let history = var_history_by_name env var_name in 
        match history with
        | None -> Printf.printf ".debug > no history for %s\n" var_name; run tl env
        | Some hist ->
            Printf.printf "BI Debug history len: %d\n" (List.length hist);
            run tl env)
      (* | "debug", _ ->
        failwith ".debug is not available for this type or expression" *)
      | _ -> failwith "Unknown builtin" )
  | Expr_st _ :: tl -> run tl env
