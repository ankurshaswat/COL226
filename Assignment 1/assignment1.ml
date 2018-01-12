type myString = EmptyString | Alphabet | Concat of (myString*myString);;

let rec lgh myString = match myString with
			EmptyString -> 0
			|  Alphabet -> 1
			|  Concat(a,b) -> (lgh a) + (lgh b);;

let rec nonempty myString = match myString with
			EmptyString -> false
			| Alphabet -> true
			| Concat(a,b) -> (nonempty a) || (nonempty b);;

let concat (a,b) = a@b;;

let reverse myString
