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
	| LBRACKET termlist RBRACKET { Function("0List",$2) }
	| LBRACKET term LISTSEPERATOR termlist RBRACKET { Function("0List",[$2]@($4)) }
	| LCURLY term COMMA term RCURLY { Function("0Pair",[$2;$4])}
	| intexpr { Function("0"^(string_of_int $1),[]) }
	| boolexpr { Function("0"^(string_of_bool $1),[]) }

intexpr: subexpr MINUS intexpr { $1-$3 }
		| subexpr {$1}
		| MINUS subexpr {(-1)*($2)}

subexpr: subexpr1 PLUS subexpr { $1+$3 }
		| subexpr1 {$1}
		| PLUS subexpr1 {$2}

subexpr1: subexpr2 MULTIPLICATION subexpr1 { $1*$3 }
		| subexpr2 {$1}

subexpr2: subexpr3 DIVISION subexpr2 { $1/$3 }
		| subexpr3 {$1}

subexpr3: subexpr4 MODULUS subexpr3 { $1 mod $3}
	| subexpr4 {$1}

subexpr4: subexpr5 EXPONENTIATION subexpr4 { int_of_float ((float_of_int $1) ** (float_of_int $3))}
	| subexpr5 {$1}



subexpr5: LPAREN intexpr RPAREN { $2 }
		| Int {$1}

bool: TRUE { true }
	| FALSE { false }

boolexpr: intexpr EQUAL intexpr { $1=$3 }
	| boolexpr EQUAL boolexpr { $1=$3 }
	| intexpr GREATER intexpr { $1>$3}
	| intexpr GREATEREQUAL intexpr { $1>=$3}
	| intexpr LESS intexpr { $1<$3}
	| intexpr LESSEQUAL intexpr { $1<=$3}

variable: Var      { Var($1) }