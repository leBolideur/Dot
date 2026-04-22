open Dot.Lexer

let () =
  (* let input = *)
  (* "MyVar = \"Hello Dot!\" myFunction == != ! =  .  0. > >= <= < !134 = " *)
  (* in *)
  let state = { index = 0; line = 1 } in
  (* let tokens = Dot.Lexer.lex input state [] in *)
  let math_tokens = Dot.Lexer.lex "(2 + 3) * 4 - 10 / 2 + (8 - 3) * 2" state [] in
  (* let math_tokens = Dot.Lexer.lex "1 + 2 * 3" state [] in *)
  (* List.iter Dot.Lexer.print_token (List.rev math_tokens); *)

  let result = Dot.Parser.parse_expr (List.rev math_tokens) in
  Printf.printf "Result >>> %d\n" result
