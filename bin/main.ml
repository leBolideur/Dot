open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input =
    "
    \   Str = \"Hello\".\n
    \   .print Str\n\
    \   .print \"World!\"\n
    \   Freeze = 4.\n
    \   VBool = 1 + 8 == ((2 + 4) / 2) * 2\n
    \   if VBool\n
    \     .print \"VBool trueee in IFFFFFF\"\n
    \     .
    \   else\n
    \      .print \"VBool faaaaalse in ELLLLLSE\"\n
    \      .\n  
    \   .print VBool\n"
  in
  let tokens = Dot.Lexer.lex input state [] in
  (* List.iter print_token (List.rev tokens); *)

  let ({ statements = stmts }, _) = Dot.Parser.parse (List.rev tokens) [] in

  let _ = run (List.rev stmts) [] in
  (* print_env env; *)

  print_endline "\n\nso far, so good!"
