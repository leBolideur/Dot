type env_entry = {
  name : string;
  freezed : bool;
  value : int;
  history : int list;
}

let rec find_in_env env var_name =
  match env with
  | [] -> None
  | entry :: _ when entry.name = var_name -> Some entry
  | _ :: tl -> find_in_env tl var_name 

let rec print_env env =
  match env with
  | [] -> print_endline "End of env"
  | { name = _; freezed = _; value = _; history = _ } :: tl ->
      print_env tl

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