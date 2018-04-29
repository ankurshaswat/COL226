all:
	ocamllex PrologLexer.mll
	ocamlyacc PrologParser.mly
	ocamlc -c PrologAbstractSyntax.ml
	ocamlc -c PrologParser.mli
	ocamlc -c PrologParser.ml
	ocamlc -c PrologLexer.ml
	ocamlc -c Prolog.ml
	ocamlmktop -o Prolog.top PrologAbstractSyntax.cmo PrologParser.cmo PrologLexer.cmo Prolog.cmo
	# ocamlmktop -o Prolog.top PrologAbstractSyntax.cmo PrologLexer.cmo Prolog.cmo

clean:
	rm *.top *.cmo *.cmi *.mli PrologLexer.ml PrologParser.ml
