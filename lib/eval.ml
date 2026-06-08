open Parser
open Env

let rec print_stmts stmts =
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
  | Call (name, args) -> (
      let fun_ = find_in_env env name in
      match fun_ with
      | None -> failwith ("No function found with that name: " ^ name)
      | Some entry -> (
          match entry.value with
          | VFun (fname, fparams, fbody, _) when fname = name ->
              let parsed_args = eval_fun_args env fparams args [] in
              let rec_entry =
                {
                  name = fname;
                  freezed = false;
                  value = VFun (fname, fparams, fbody, parsed_args);
                  history = [];
                }
              in
              let rec_args = Entry rec_entry :: parsed_args in
              let ret, _ = run_list (List.rev fbody) (rec_args) in
              ret
          | _ -> failwith "Not a function!"))
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

and eval_fun_args env params args acc =
  match (params, args) with
  | [], [] -> List.rev acc
  | hd :: tl, hd' :: tl' ->
      let parsed = eval_ast hd' env in
      let entry =
        { name = hd; freezed = false; value = parsed; history = [] }
      in
      eval_fun_args env tl tl' (Entry entry :: acc)
  | _, _ -> failwith "Error on eval args"

and run_list stmts env =
  match stmts with
  | [] -> (VUnit, env)
  | last :: [] -> 
    let ret_value, ret_env = process_stmt last env in
    (ret_value, ret_env)
  | hd :: tl ->
      let _, env_inter = process_stmt hd env in
      run_list tl env_inter

and process_stmt stmt env =
  match stmt with
  | Decl_st (var_name, ast, is_freezed) ->
      let new_env =
        match find_in_local_env env var_name with
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
            Entry var :: env
        | Some _ when is_freezed -> failwith "Variable already freezed"
        | Some entry ->
            let updated_var = update_variable (eval_ast ast env) entry in
            let new_env = replace_in_local_env env updated_var in
            new_env
      in
      (VUnit, new_env)
  | Freeze_st name ->
      let var_to_freeze =
        match find_in_local_env env name with
        | Some entry -> entry
        | None -> failwith ("Unable to freeze this var: " ^ name)
      in
      let new_var = { var_to_freeze with freezed = true } in
      let new_env = replace_in_local_env env new_var in

      (VUnit, (Entry new_var :: new_env))
  | Expr_Builtin_st (name, ast) -> (
      let node = eval_ast ast env in
      match (name, node) with
      | "print", VInt nb ->
          Printf.printf "%d\n" nb;
          (VUnit, env)
      | "print", VStr str ->
          Printf.printf "%s\n" str;
          (VUnit, env)
      | "print", VBool boolean ->
          Printf.printf "%b\n" boolean;
          (VUnit, env)
      | "print", VIdent ident ->
        let value = find_in_env env ident in
        (match value with
        | Some v -> 
            print_value v.value;
             (VUnit, env)
        | _ -> 
            failwith "Not able to print this type")
           
      | _ -> failwith "Unknown print builtin")
  | Stmt_Builtin_st (name, ident) -> (
      match (name, ident) with
      | "debug", var_name -> (
          let history = var_history_by_name env var_name in
          match history with
          | None ->
              print_env env;
              Printf.printf ".debug > no history for %s\n" var_name;
              (VUnit, env)
          | Some hist ->
              Printf.printf ".debug > history len for %s : %d\n" var_name
                (List.length hist);
              print_history var_name hist 0;
              Printf.printf "Is freezed? %b\n"
                (is_var_name_freezed env var_name);
              (VUnit, env))
      | _ -> failwith "Unknown debug builtin")
  | Expr_st expr -> (eval_ast expr env, env)
  | If_st (cond, consequence, alternative) -> (
      let eval_cond = eval_ast cond env in
      match eval_cond with
      | VBool true -> run_list (List.rev consequence) (ScopeMarker :: env)
      | _ -> run_list (List.rev alternative) (ScopeMarker :: env))
  | Func_st (name, params, body) ->
      let vfun = VFun (name, params, body, env) in
      let entry = { name; freezed = false; value = vfun; history = [] } in
      let fenv = (Entry entry :: ScopeMarker :: env) in
      (VUnit, fenv)
  | Call_st (fun_name, args) -> (
      let fun_ = find_in_env env fun_name in
      match fun_ with
      | None -> failwith ("No function found with that name: " ^ fun_name)
      | Some entry -> (
          match entry.value with
          | VFun (fname, fparams, fbody, _) when fname = fun_name ->
              let parsed_args = eval_fun_args env fparams args [] in
              let rec_entry =
                {
                  name = fname;
                  freezed = false;
                  value = VFun (fname, fparams, fbody, parsed_args);
                  history = [];
                }
              in
              let rec_args = Entry rec_entry :: parsed_args in
              run_list (List.rev fbody) (rec_args)
          | _ -> failwith "Not a function!"))