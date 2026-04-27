open Parser

type env_entry = {
  name : string;
  freezed : bool;
  value : int;
  history : int list;
}

let rec find_in_env var_name env =
  match env with
  | [] -> None
  | entry :: _ when entry.name = var_name -> Some entry
  | _ :: tl -> find_in_env var_name tl

let rec print_env env =
  match env with
  | [] -> print_endline "End of env"
  | { name = var_name; freezed = var_freezed; value = var_value; history = _ }
    :: tl ->
      Printf.printf "VAR %s = %d\tfreezed? %b\n" var_name var_value var_freezed;
      print_env tl

let rec eval_ast ast env =
  match ast with
  | Int nb -> nb
  | Var name -> (
      match find_in_env name env with
      | None ->
          Printf.printf "Unbound variable %s\n" name;
          failwith ""
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
      let new_var =
        {
          name = var.name;
          freezed = false;
          value = new_value;
          history = new_history;
        }
      in
      Printf.printf "Update var for %s >>> %d\n" var.name new_var.value;
      new_var

let rec print_var_history var index =
  match var.history with
  | [] -> print_endline "No history"
  | _ :: _ when index >= List.length var.history -> print_endline "End history"
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
  | Decl_st (var_name, ast, freezed) :: tl ->
      let new_env =
        match find_in_env var_name env with
        | None ->
            Printf.printf "%s not existing in env\n" var_name;
            let value = eval_ast ast env in
            let var =
              { name = var_name; freezed = false; value; history = value :: [] }
            in
            (* Printf.printf "Eval Decl_st for %s >>> %d\n" var_name var.value; *)
            (* print_env env; *)
            var :: env
        | Some entry when freezed ->
            Printf.printf "%s Exist with value: %d BUT freezed = %b\n"
              entry.name entry.value entry.freezed;
            failwith "Frrrrreeeeezed"
        | Some entry ->
            Printf.printf "%s Exist with value: %d AND freezed = %b\n"
              entry.name entry.value entry.freezed;
            let updated_var = update_variable (eval_ast ast env) entry in
            print_endline "\nHistory:";
            print_var_history updated_var 0;
            (* print_env env; *)
            let new_env = pop_env_by_var_name env updated_var.name in
            updated_var :: new_env
      in
      eval tl new_env
  | Expr_st ast :: tl ->
      Printf.printf "Eval Expr_st >>> %d\n" (eval_ast ast env);
      eval tl env
