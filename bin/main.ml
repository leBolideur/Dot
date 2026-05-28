open Dot.Lexer
open Dot.Parser
open Dot.Eval


let () =
  let state = { index = 0; line = 1 } in

  let input = "
A = 10
B = 5
Sum = A + B
Diff = A - B
Prod = A * B
Div = A / B

.print A
X = 100
X.

if A == 10
  X = 50
  .print \"TEST 5 - X dans if\"
  .
else
  .print \"TEST 5 - ELSE\"
  .

.print \"TEST 5 - X apres if\"

Global = \"Je suis global\"

if 1 == 1
  Local = \"Je suis local\"
  .print \"TEST 6 - Local dans if\"
  .print \"TEST 6 - Global dans if\"
  .
else
  .print \"TEST 6 - ELSE\"
  .

.print \"TEST 6 - Global apres if\"

Y = 1
if Y == 1
  Y = 2
  if Y == 2
    Y = 3
    .print \"TEST 7 - Y niveau 3\"
    .
  .print \"TEST 7 - Y niveau 2\"
  .
.print \"TEST 7 - Y niveau 1\"

if (1 + 1) == 2
  .print \"TEST 8 - Math dans condition OK\"
  .

if (A * B) == 50
  .print \"TEST 8 - Comparaison complexe OK\"
  .

.print \"TEST 9 - String simple\"

Start = 10
if Start == 10
  Middle = 20
  End = 30
  .print \"TEST 10 - Middle\"
  .print \"TEST 10 - End\"
  .
.print \"TEST 10 - Start\"

if 1 == 1
  .print \"TEST 11 - Bool fonctionne\"
  .

Priority = 2 + 3 * 4
.print \"TEST 12 - Priorite\"

Parentheses = (2 + 3) * 4
.print \"TEST 12 - Parentheses\"

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
