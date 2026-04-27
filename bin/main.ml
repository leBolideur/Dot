open Dot.Lexer
open Dot.Parser

let () =
  (* let input = *)
  (* "MyVar = \"Hello Dot!\" myFunction == != ! =  .  0. > >= <= < !134 = " *)
  (* in *)
  let state = { index = 0; line = 1 } in
  (* let tokens = Dot.Lexer.lex input state [] in *)

  (* let math_tokens = Dot.Lexer.lex "(2 + 3) * 4 - 10 / 2 + (8 - 3) * 2" state [] in *)

  (* let input = "X = 2 + 2 * 3 + 2\n    X\n    Y = 7 * 7\n    Z = X + Y" in *)
  let input = "Y = 7 * 7\nTest = 2 + 2*3+2\nTotal = Y + Test + 1\n 2+2\nTotal = 666" in
  let var_tokens = Dot.Lexer.lex input state [] in
  (* List.iter Dot.Lexer.print_token (List.rev var_tokens); *)

  let {statements = stmts} = Dot.Parser.parse (List.rev var_tokens) [] in
  Printf.printf "stmts len. = %d\n" (List.length stmts);
  let env = Dot.Parser.eval (List.rev stmts) [] in
  Dot.Parser.print_env env;

  print_endline "so far, so good!"
