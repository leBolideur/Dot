open Dot.Lexer

let () =
  (* let input = *)
  (* "MyVar = \"Hello Dot!\" myFunction == != ! =  .  0. > >= <= < !134 = " *)
  (* in *)
  let state = { index = 0; line = 1 } in
  (* let tokens = Dot.Lexer.lex input state [] in *)

  (* let math_tokens = Dot.Lexer.lex "(2 + 3) * 4 - 10 / 2 + (8 - 3) * 2" state [] in *)

  let input = "X = 2 + 2 * 3 + 2
    X
    Y = 7 * 7
    Z = X + Y" in
  let var_tokens = Dot.Lexer.lex input state [] in
  List.iter Dot.Lexer.print_token (List.rev var_tokens);

  let Program statements = Dot.Parser.parse (List.rev var_tokens) [] in
  Printf.printf "stmts len. = %d\n" (List.length statements);
  let _ = Dot.Parser.eval (List.rev statements) [] in
  (* Printf.printf "Result >>> %d\n" result *)

  print_endline "so far, so good!"
