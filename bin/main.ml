open Dot.Lexer

let () =
  let input =
    "MyVar = \"Hello Dot!\" myFunction == != ! =  .  0. > >= <= < !134 = "
  in
  let state = { index = 0; line = 1 } in
  let tokens = Dot.Lexer.lex input state [] in
  List.iter Dot.Lexer.print_token (List.rev tokens); Dot.Parser.parse_expr
