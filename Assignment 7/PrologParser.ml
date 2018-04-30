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

open Parsing;;
let _ = parse_error;;
# 2 "PrologParser.mly"
open PrologAbstractSyntax
# 36 "PrologParser.ml"
let yytransl_const = [|
  260 (* RULE_IMPLICATION *);
  261 (* COMMA *);
  262 (* PERIOD *);
  263 (* SEMICOLON *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* LBRACKET *);
  267 (* RBRACKET *);
  268 (* LCURLY *);
  269 (* RCURLY *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* MULTIPLICATION *);
  273 (* DIVISION *);
  274 (* MODULUS *);
  275 (* EXPONENTIATION *);
  276 (* TRUE *);
  277 (* FALSE *);
  278 (* EQUAL *);
  279 (* GREATER *);
  280 (* LESS *);
  281 (* GREATEREQUAL *);
  282 (* LESSEQUAL *);
    0 (* EOF *);
  283 (* LISTSEPERATOR *);
    0|]

let yytransl_block = [|
  257 (* Const *);
  258 (* Var *);
  259 (* Int *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\002\000\002\000\004\000\006\000\
\006\000\005\000\005\000\005\000\005\000\005\000\005\000\007\000\
\000\000\000\000"

let yylen = "\002\000\
\003\000\002\000\003\000\001\000\003\000\001\000\001\000\003\000\
\001\000\001\000\004\000\001\000\003\000\005\000\005\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\000\000\017\000\
\000\000\000\000\007\000\010\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\001\000\003\000\005\000\011\000\008\000\
\000\000\000\000\014\000\015\000"

let yydgoto = "\003\000\
\008\000\013\000\009\000\014\000\011\000\017\000\012\000"

let yysindex = "\009\000\
\007\255\007\255\000\000\252\254\000\000\007\255\007\255\000\000\
\015\255\023\255\000\000\000\000\000\000\024\255\007\255\251\254\
\017\255\025\255\007\255\007\255\007\255\026\255\027\255\007\255\
\007\255\000\000\007\255\000\000\000\000\000\000\000\000\000\000\
\021\255\020\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\028\255\000\000\000\000\000\000\002\000\000\000\029\255\
\000\000\000\000\035\000\000\000\000\000\009\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\018\000\251\255\000\000\006\000\255\255\244\255\000\000"

let yytablesize = 284
let yytable = "\024\000\
\012\000\006\000\023\000\015\000\016\000\018\000\010\000\004\000\
\005\000\001\000\002\000\032\000\033\000\022\000\029\000\030\000\
\006\000\009\000\007\000\009\000\019\000\025\000\022\000\022\000\
\010\000\034\000\020\000\026\000\021\000\027\000\024\000\035\000\
\036\000\004\000\002\000\031\000\028\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\012\000\012\000\006\000\
\000\000\012\000\000\000\012\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000"

let yycheck = "\005\001\
\000\000\000\000\015\000\008\001\006\000\007\000\001\000\001\001\
\002\001\001\000\002\000\024\000\025\000\015\000\020\000\021\000\
\010\001\009\001\012\001\011\001\006\001\027\001\024\000\025\000\
\019\000\027\000\004\001\011\001\005\001\005\001\005\001\011\001\
\013\001\006\001\000\000\009\001\019\000\255\255\255\255\011\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\006\001\
\255\255\009\001\255\255\011\001\255\255\013\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001"

let yynames_const = "\
  RULE_IMPLICATION\000\
  COMMA\000\
  PERIOD\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LCURLY\000\
  RCURLY\000\
  PLUS\000\
  MINUS\000\
  MULTIPLICATION\000\
  DIVISION\000\
  MODULUS\000\
  EXPONENTIATION\000\
  TRUE\000\
  FALSE\000\
  EQUAL\000\
  GREATER\000\
  LESS\000\
  GREATEREQUAL\000\
  LESSEQUAL\000\
  EOF\000\
  LISTSEPERATOR\000\
  "

let yynames_block = "\
  Const\000\
  Var\000\
  Int\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : PrologAbstractSyntax.program) in
    Obj.repr(
# 36 "PrologParser.mly"
                                      ( [_1]@(_3) )
# 227 "PrologParser.ml"
               : PrologAbstractSyntax.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 37 "PrologParser.mly"
                          ( [_1] )
# 234 "PrologParser.ml"
               : PrologAbstractSyntax.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : PrologAbstractSyntax.goal) in
    Obj.repr(
# 39 "PrologParser.mly"
                                                         ( Rule(_1,_3) )
# 242 "PrologParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicformula) in
    Obj.repr(
# 40 "PrologParser.mly"
                             ( Fact(_1) )
# 249 "PrologParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : PrologAbstractSyntax.goal) in
    Obj.repr(
# 42 "PrologParser.mly"
                                                         ( [_1]@(_3) )
# 257 "PrologParser.ml"
               : PrologAbstractSyntax.goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicformula) in
    Obj.repr(
# 43 "PrologParser.mly"
                              ( [_1] )
# 264 "PrologParser.ml"
               : PrologAbstractSyntax.goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 45 "PrologParser.mly"
                    ( _1 )
# 271 "PrologParser.ml"
               : 'atomicformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 47 "PrologParser.mly"
                              ( [_1]@(_3) )
# 279 "PrologParser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "PrologParser.mly"
        ( [_1] )
# 286 "PrologParser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 51 "PrologParser.mly"
                 ( V(_1) )
# 293 "PrologParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 52 "PrologParser.mly"
                                   ( Function(_1,_3) )
# 301 "PrologParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "PrologParser.mly"
         ( Function(_1,[]) )
# 308 "PrologParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 54 "PrologParser.mly"
                              ( Function("List",_2) )
# 315 "PrologParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 55 "PrologParser.mly"
                                                 ( Function("List",[_2]@(_4)) )
# 323 "PrologParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 56 "PrologParser.mly"
                                 ( Function("Pair",[_2;_4]))
# 331 "PrologParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "PrologParser.mly"
                   ( Var(_1) )
# 338 "PrologParser.ml"
               : 'variable))
(* Entry clauselist *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry atomicformulalist *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let clauselist (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : PrologAbstractSyntax.program)
let atomicformulalist (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : PrologAbstractSyntax.goal)
