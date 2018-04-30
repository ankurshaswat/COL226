type token =
  | Const of (string)
  | Var of (string)
  | Int of (int)
  | RULE_IMPLICATION
  | COMMA
  | PERIOD
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LCURLY
  | RCURLY
  | PLUS
  | MINUS
  | MULTIPLICATION
  | DIVISION
  | MODULUS
  | EXPONENTIATION
  | TRUE
  | FALSE
  | EQUAL
  | GREATER
  | LESS
  | GREATEREQUAL
  | LESSEQUAL
  | EOF
  | LISTSEPERATOR

val clauselist :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> PrologAbstractSyntax.program
val atomicformulalist :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> PrologAbstractSyntax.goal
