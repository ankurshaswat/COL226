{
open PrologParser
}

rule token = parse
  | [' ' '\t' '\n']    { token lexbuf } (* skip spaces *)

  | ":-"                { RULE_IMPLICATION }
  | ","                 { COMMA }
  | "."                 { PERIOD }
  | ";"                 { SEMICOLON }

  | ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str { Const(str) }
  | ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str { Var(str) }

  | "("                { LPAREN }
  | ")"                { RPAREN }

  | "["                { LBRACKET}
  | "|"                { LISTSEPERATOR}
  | "]"                { RBRACKET}

  | "{"                { LCURLY }
  | "}"                { RCURLY }

  | ['0'-'9']+         as int { Int(int_of_string int) }

  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULTIPLICATION }
  | "div" { DIVISION }
  | "mod" { MODULUS }
  | "^" { EXPONENTIATION }

  | "T" { TRUE }
  | "F" { FALSE }

  | "=" { EQUAL }
  | ">" { GREATER }
  | "<" { LESS }
  | ">=" { GREATEREQUAL }
  | "<=" { LESSEQUAL }

  | _ as chr           { failwith ("lex error: "^(Char.escaped chr))}
  | eof                { EOF }

