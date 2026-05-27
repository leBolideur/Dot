open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input = "
X = 100

if 1 == 1
  X = 50
  .print \"TEST 5 - X dans if\"
  .
else
  .print \"TEST 5 - ELSE\"
  .

    "
  in
  let tokens = Dot.Lexer.lex input state [] in
  List.iter print_token (List.rev tokens);

  let ({ statements = stmts }, _) = Dot.Parser.parse (List.rev tokens) [] in

  let _ = run (List.rev stmts) [] in
  (* print_env env; *)

  print_endline "\n\nso far, so good!"
