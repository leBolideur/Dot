open Dot.Lexer

let () =
  (* let input = *)
  (* "MyVar = \"Hello Dot!\" myFunction == != ! =  .  0. > >= <= < !134 = " *)
  (* in *)
  let state = { index = 0; line = 1 } in
  (* let tokens = Dot.Lexer.lex input state [] in *)

  (* let math_tokens = Dot.Lexer.lex "(2 + 3) * 4 - 10 / 2 + (8 - 3) * 2" state [] in *)

  let input = "X = 2 + 2
    X" in
  let var_tokens = Dot.Lexer.lex input state [] in

  let statements  = Dot.Parser.parse (List.rev var_tokens) [] in
  Printf.printf "stmts len. = %d\n" (List.length statements);
  let _ = Dot.Parser.eval statements [] in
  (* Printf.printf "Result >>> %d\n" result *)

  print_endline "so far, so good!"
