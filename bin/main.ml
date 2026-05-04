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

   Y.\n
   Str = \"Hello\".
   
   Freeze = 4." in
  let var_tokens = Dot.Lexer.lex input state [] in

  let {statements = stmts} = Dot.Parser.parse (List.rev var_tokens) [] in
 
  let env = eval (List.rev stmts) [] in
  print_env env;

  print_endline "so far, so good!"
