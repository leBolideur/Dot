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

.print \"TEST 1 - Maths\"
.print Sum
.print Diff
.print Prod
.print Div

Eq = A == B
NotEq = B == A

.print \"TEST 2 - Comparaisons\"
.print Eq
.print NotEq

if A == 10
  .print \"TEST 3 - A vaut 10\"
  .
else
  .print \"TEST 3 - A ne vaut pas 10\"
  .

if B == 5
  .print \"TEST 3 - B vaut 5\"
  .
else
  .print \"TEST 3 - B ne vaut pas 5\"
  .

if A == 10
  if B == 5
    .print \"TEST 4 - A vaut 10 ET B vaut 5\"
    .
  else
    .print \"TEST 4 - A vaut 10 MAIS B ne vaut pas 5\"
    .
  .
else
  .print \"TEST 4 - A ne vaut pas 10\"
  .

X = 100
X.

if true
  X = 50
  .print \"TEST 5 - X dans if\"
  .
.print \"TEST 5 - X apres if\"

Global = \"Je suis global\"

if true
  Local = \"Je suis local\"
  .print \"TEST 6 - Local dans if\"
  .print \"TEST 6 - Global dans if\"
  .

.print \"TEST 6 - Global apres if\"

Y = 1
if true
  Y = 2
  if true
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

if true == true
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
