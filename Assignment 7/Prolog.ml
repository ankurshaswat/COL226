let parse_string_goals str =
	let lb = Lexing.from_string str
in 
	PrologParser.atomicformulalist PrologLexer.token lb

let parse_string_program str =
	let lb = Lexing.from_string str
	in
		PrologParser.clauselist PrologLexer.token lb
	
let parse_channel_program=
	let lb = Lexing.from_channel (open_in Sys.argv.(1))
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

let rec print_term_list tl = match tl with
| x::xs -> Printf.printf ","; print_term x;print_term_list xs
| _ -> ()
and print_term tl = match tl with
| PrologAbstractSyntax.Function(s,[]) -> Printf.printf "%s" s
| PrologAbstractSyntax.Function(s,x::xs) -> Printf.printf "%s" s;
										Printf.printf "(" ; 
										print_term x;
										print_term_list xs;
										Printf.printf ")";
| PrologAbstractSyntax.V(PrologAbstractSyntax.Var(s)) -> Printf.printf "%s" s

let rec print_goals gg = match gg with
| x::xs -> (print_term x); print_goals xs ; Printf.printf "\n"
| _ -> ()

let rec print_res g = match g with
| x::xs -> (print_goals x); (print_res xs)
| [] -> ()

let _ = Printf.printf "?-"; flush stdout;
	let lb =Lexing.from_channel stdin in 
	while true do
		let goals = PrologParser.atomicformulalist PrologLexer.token lb in

			match (PrologAbstractSyntax.run_prolog goals (parse_channel_program)) with
			| ress-> print_res ress;Printf.printf "\n";flush stdout;Printf.printf "?-"; flush stdout;
	done
