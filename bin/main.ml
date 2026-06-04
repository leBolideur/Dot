open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input = "
add(Nb1, Nb2) ->
  N = Nb1 + Nb2.
  N
  .

Res = add(10, 9)
.print Res
    "
  in
  let tokens = Dot.Lexer.lex input state [] in
  (*List.iter print_token (List.rev tokens);*)

  let (program, _) = Dot.Parser.parse tokens [] in

  let (_, _) = run_list program.statements [] in
  (*Dot.Env.print_value ret_value;*)
  (* print_env env; *)

  print_endline "\n\nso far, so good!"
