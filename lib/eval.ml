open Parser
open Env

let rec eval_ast ast env =
  match ast with
  | Int nb -> nb
  | Var name -> (
      match find_in_env env name with
      | None ->
          Printf.printf "Unbound variable %s\n" name;
          failwith ""
      | Some entry -> entry.value)
  | Add (left, right) -> eval_ast left env + eval_ast right env
  | Minus (left, right) -> eval_ast left env - eval_ast right env
  | Mul (left, right) -> eval_ast left env * eval_ast right env
  | Div (left, right) -> eval_ast left env / eval_ast right env

let rec eval program_stmts env =
  match program_stmts with
  | [] -> env
  | Decl_st (var_name, ast, is_freezed) :: tl ->
      let new_env =
        match find_in_env env var_name with
        | None ->
            Printf.printf "%s not existing in env\n" var_name;
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
        | Some entry when is_freezed ->
            Printf.printf "%s Exist with value: %d BUT freezed = %b\n"
              entry.name entry.value entry.freezed;
            failwith "Frrrrreeeeezed var error"
        | Some entry ->
            Printf.printf "%s Exist with value: %d AND freezed = %b\n"
              entry.name entry.value entry.freezed;

            let updated_var = update_variable (eval_ast ast env) entry in

            print_endline "\nHistory:";
            print_var_history updated_var 0;

            let new_env = pop_env_by_var_name env updated_var.name in
            updated_var :: new_env
      in
      eval tl new_env
  | Freeze_st name :: tl ->
    print_endline "env before Freeze_st";
    print_env env;
    let var_to_freeze = 
      match find_in_env env name with
      | Some entry -> entry
      | None -> failwith "Unable to freeze this var"
       in
    let new_var = {var_to_freeze with freezed = true} in
    let new_env = pop_env_by_var_name env name in
    print_endline "env after Freeze_st";
    print_env new_env;
    eval tl (new_var :: new_env)
  | Expr_st ast :: tl ->
      Printf.printf "Eval Expr_st >>> %d\n" (eval_ast ast env);
      eval tl env
