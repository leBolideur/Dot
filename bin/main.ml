open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input =
    "Y = 7 * 7\n\n\
    \  Test = 2 + 2*3+2\n\n\
    \  Test = Test + 1\n
    \  Test = Test - 1\n
    \  Test = 100\n
    \  Test = 50\n
    \  Total = Y + Test\n\n\
    \  .debug Total
    \   \n\
    \   .print Total\n\
    \   \n\n\
    \   Str = \"Hello\".\n\
    \   .print Str\n\
    \   .print \"World!\"\n\
    \   .debug Test\n\
    \   Freeze = 4."
  in
  let tokens = Dot.Lexer.lex input state [] in
  (* List.iter print_token (List.rev tokens); *)

  let { statements = stmts } = Dot.Parser.parse (List.rev tokens) [] in

  let _ = run (List.rev stmts) [] in
  (* print_env env; *)

  print_endline "so far, so good!"
