open Parser
open Env

(* type value = VInt of int | VStr of string *)

let rec eval_ast ast env =
  match ast with
  | IntLit nb -> VInt nb
  | StrLit str -> VStr str
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

let rec eval program_stmts env =
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
      eval tl new_env
  | Freeze_st name :: tl ->
      print_env env;
      let var_to_freeze =
        match find_in_env env name with
        | Some entry -> entry
        | None -> failwith ("Unable to freeze this var: " ^ name)
      in
      let new_var = { var_to_freeze with freezed = true } in
      let new_env = pop_env_by_var_name env name in

      print_env new_env;
      eval tl (new_var :: new_env)
  | Expr_st _ :: tl -> eval tl env
