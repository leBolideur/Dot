open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input = "
add(Nb1, Nb2) ->
  Res = Nb1 + Nb2
  Res
  .

mul(Nb1, Nb2) ->
  Res = Nb1 * Nb2
  Res
  .

Res = mul(10, 2)
.print Res
Res2 = add(Res, 50)
.print Res2
"
  in
  let tokens = Dot.Lexer.lex input state [] in
  (*List.iter print_token (List.rev tokens);*)

  let (program, _) = Dot.Parser.parse tokens [] in

  let (_, _) = run_list program.statements [] in
  (*print_endline "stmts:";
  print_stmts program.statements;
  print_endline "endddd";
  Dot.Env.print_value ret_value;*)
  (* print_env env; *)

  print_endline "\n\nso far, so good!"
