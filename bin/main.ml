open Dot.Lexer
open Dot.Parser
open Dot.Eval
open Dot.Env

let () =
  let state = { index = 0; line = 1 } in
 
  let input = "Y = 7 * 7\n
  Test = 2 + 2*3+2\n
  Total = Y + Test + 1\n
   
   Total = 666
   Total = 7736
   .print Total
   

   Str = \"Hello\".
   .print Str
   .print \"World!\"
   .debug Test
   Freeze = 4." in
  let tokens = Dot.Lexer.lex input state [] in
  (* List.iter print_token (List.rev tokens); *)

  let {statements = stmts} = Dot.Parser.parse (List.rev tokens) [] in
   
  let env = run (List.rev stmts) [] in
  print_env env;

  print_endline "so far, so good!"
