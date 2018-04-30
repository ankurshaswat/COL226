%{
open PrologAbstractSyntax
%}

%token <string> Const
%token <string> Var
%token <int> Int
%token RULE_IMPLICATION
%token COMMA
%token PERIOD
%token SEMICOLON
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token PLUS
%token MINUS
%token MULTIPLICATION
%token DIVISION
%token MODULUS
%token EXPONENTIATION
%token TRUE FALSE
%token EQUAL
%token GREATER
%token LESS
%token GREATEREQUAL
%token LESSEQUAL
%token EOF
%token LISTSEPERATOR

%start clauselist
%type <PrologAbstractSyntax.program> clauselist
%start atomicformulalist
%type <PrologAbstractSyntax.goal> atomicformulalist
%%

clauselist: clause PERIOD clauselist  { [$1]@($3) }
    | clause PERIOD       { [$1] }

clause: atomicformula RULE_IMPLICATION atomicformulalist { Rule($1,$3) }
    | atomicformula          { Fact($1) }

atomicformulalist: atomicformula COMMA atomicformulalist { [$1]@($3) }
    | atomicformula           { [$1] }

atomicformula: term { $1 }

termlist: term COMMA termlist { [$1]@($3) }
	| term { [$1] }


term:  variable  { V($1) }
	|  Const LPAREN termlist RPAREN   { Function($1,$3) }
	| Const { Function($1,[]) }
	| LBRACKET termlist RBRACKET { Function("List",$2) }
	| LBRACKET term LISTSEPERATOR termlist RBRACKET { Function("List",[$2]@($4)) }
	| LCURLY term COMMA term RCURLY { Function("Pair",[$2;$4])}

variable: Var      { Var($1) }