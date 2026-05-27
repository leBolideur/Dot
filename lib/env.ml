open Parser

type env_entry = {
  name : string;
  freezed : bool;
  value : value;
  history : value list;
}

type env = Entry of env_entry | ScopeMarker

let rec print_env env =
  match env with
  | [] -> print_endline "End of env"
  | Entry { name; freezed; value; history = var_hist } :: tl -> (
      match value with
      | VInt nb ->
          Printf.printf "VInt - %s = %d is freezed? %b\thistory len = %d\n" name
            nb freezed (List.length var_hist);
          print_env tl
      | VStr str ->
          Printf.printf "VStr - %s = %s is freezed? %b\n" name str freezed;
          print_env tl
      | _ -> print_env tl)
  | ScopeMarker :: tl ->
      print_endline "Scope Marker";
      print_env tl

(*let rec find_in_env env var_name =
  match env with
  | [] -> None
  (*| ScopeMarker :: _ -> Some ScopeMarker*)
  | Entry entry :: _ when entry.name = var_name -> Some entry
  | _ :: tl -> find_in_env tl var_name*)

let rec find_in_local_env env var_name =
  match env with
  | [] -> None
  | ScopeMarker :: _ -> None
  | Entry entry :: _ when entry.name = var_name -> Some entry
  | _ :: tl -> find_in_local_env tl var_name

let rec find_in_env env var_name =
  match env with
  | [] -> None
  | ScopeMarker :: tl -> find_in_env tl var_name
  | Entry entry :: _ when entry.name = var_name -> Some entry
  | _ :: tl -> find_in_env tl var_name

let update_variable ast var =
  match var.freezed with
  | true -> failwith "Freezed variable! Update is forbidden"
  | false ->
      let new_history = var.history @ [ ast ] in
      let new_var =
        { name = var.name; freezed = false; value = ast; history = new_history }
      in
      new_var

let rec print_var_history var index =
  match var.history with
  | [] -> print_endline "No history"
  | _ :: _ when index >= List.length var.history -> print_endline "End history"
  | _ :: _ -> (
      match List.nth var.history index with
      | VInt number ->
          Printf.printf "%s@%d = %d\n" var.name index number;
          print_var_history var (index + 1)
      | VStr str ->
          Printf.printf "%s@%d = %s\n" var.name index str;
          print_var_history var (index + 1)
      | _ -> print_var_history var (index + 1))

let rec print_history var_name history index =
  match history with
  | [] -> print_endline "No history"
  | _ :: _ when index >= List.length history -> print_endline "End history"
  | _ :: _ -> (
      match List.nth history index with
      | VInt number ->
          Printf.printf "%s@%d = %d\n" var_name index number;
          print_history var_name history (index + 1)
      | VStr str ->
          Printf.printf "%s@%d = %s\n" var_name index str;
          print_history var_name history (index + 1)
      | VBool boolean ->
          Printf.printf "%s@%d = %b\n" var_name index boolean;
          print_history var_name history (index + 1)
      | _ -> print_history var_name history (index + 1))

let rec pop_env_by_var_name env name =
  match env with
  | [] -> []
  | Entry hd :: tl when hd.name == name -> tl
  | _ :: tl -> pop_env_by_var_name tl name

let rec var_history_by_name env name =
  match env with
  | [] -> None
  | Entry hd :: _ when hd.name = name -> Some hd.history
  | _ :: tl -> var_history_by_name tl name

let rec is_var_name_freezed env name =
  match env with
  | [] -> false
  | Entry hd :: _ when hd.name = name -> hd.freezed
  | _ :: tl -> is_var_name_freezed tl name
