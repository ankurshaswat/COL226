
let parse_string str =
	let lb = Lexing.from_string str
	in
		PrologParser.clauselist PrologLexer.token lb
		
let token_list_of_string s =
	let lb = Lexing.from_string s in
	let rec helper l =
		try
			let t = PrologLexer.token lb in
			if t = PrologParser.EOF then List.rev l else helper (t::l)
		with _ -> List.rev l
	in 
		helper []
