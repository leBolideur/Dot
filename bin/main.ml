open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input = "
test_func(Bar) ->
 Add = 1 + 5 
 Add.
 if Add == 3
  .print Add
  .
 else
  .print \"Nooooot 3\"
  .
 .

hello(Str) ->
  .print \"Hello \"
  .print Str
  .

add(Nb1, Nb2) ->
  .print Nb1 + Nb2
  .

Abc = 6.
add(Abc, 4)
add(Abc, 10)

hello(\"World\")
hello(\"Maxime\")

.print \"========================================\"
.print \"Tous les tests termines\"
.print \"Si vous voyez ce message, DotLang v0.1 est operationnel\"
.print \"========================================\"
    "
  in
  let tokens = Dot.Lexer.lex input state [] in
  (*List.iter print_token (List.rev tokens);*)

  let ({ statements = stmts }, _) = Dot.Parser.parse (List.rev tokens) [] in

  let _ = run (List.rev stmts) [] in
  (* print_env env; *)

  print_endline "\n\nso far, so good!"
