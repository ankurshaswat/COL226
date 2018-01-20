(* type 'a myString = EmptyString | Alphabet of 'a | Concatenate of ('a myString*'a myString);; *)

exception Empty;;
exception AtLast;;
exception AtFirst;;
exception TooShort;;
exception Error;;
open List;;
(*
let rec lgh myString = match myString with
			EmptyString -> 0
			|  Alphabet (a)-> 1
			|  Concatenate (a,b) -> (lgh a) + (lgh b);;

let rec nonempty myString = match myString with
			EmptyString -> false
			| Alphabet (a)-> true
			| Concatenate (a,b) -> (nonempty a) || (nonempty b);;

let concat (a,b) = match (a,b) with
			(a,EmptyString) -> a
			| (EmptyString,b) -> b
			|  (a,b) -> Concatenate (a,b) ;;

let rec reverse myString = match myString with
			EmptyString -> EmptyString
			| Alphabet (a)-> Alphabet (a)
			| Concatenate (a,b) -> Concatenate (reverse b,reverse a);;

let rec first myString = match myString with
			EmptyString -> raise Empty
			| Alphabet (a) -> a
			| Concatenate (a,b) -> first a;;

let rec last myString = match myString with
			EmptyString -> raise Empty
			| Alphabet (a) -> a
			| Concatenate (a,b) -> last b;;
 *)
(* 
type 'a newString= EmptyString | Alphabet of 'a | Concatenate of ('a list*'a list) ;;

let lgh newString = match newString with
			EmptyString -> 0
			|  Alphabet (a)-> 1
			|  Concatenate (a,b) -> (length a) + (length b);;

let nonempty newString = match newString with
			EmptyString -> false
			| Alphabet (a)-> true
			| Concatenate (a,b) -> ((length a)>0) || ((length b)>0);;

let concat a b = match (a,b) with
			(a,EmptyString) -> a
			| (EmptyString,b) -> b
			|  (Alphabet(e1),Alphabet(e2)) -> Concatenate ([],[e1;e2])
			| (Alphabet(e1),Concatenate(a1,b1)) ->  Concatenate([],[e1]@a1@b1)
			| (Concatenate(a1,b1),Alphabet(e1)) ->  Concatenate([],a1@b1@[e1])
			| (Concatenate(a1,b1),Concatenate(a2,b2)) ->  Concatenate([],a1@b1@a2@b2) ;;
 *)

 type 'a myString = EmptyString | Alphabet of 'a | Combine of ('a list * 'a list * 'a * 'a);;

 let rec rev a = match a with
 				[] ->  []
 				| x::xs -> (rev xs) @ [x];;

 let lgh myString = match myString with
			EmptyString -> 0
			|  Alphabet (a)-> 1
			|  Combine (a,b,c,d) -> (length a) + (length b);;


let nonempty myString = match myString with
			EmptyString -> false
			| _ -> true;;


let concat a b = match (a,b) with
			(a,EmptyString) -> a
			| (EmptyString,b) -> b
			|  (Alphabet(e1),Alphabet(e2)) -> Combine ([e1],[e2],e1,e2)
			| (Alphabet(e1),Combine(a1,b1,c1,d1)) ->  Combine(a1@[e1],b1,e1,d1)
			| (Combine(a1,b1,c1,d1),Alphabet(e1)) ->  Combine(a1,b1@[e1],c1,e1)
			| (Combine(a1,b1,c1,d1),Combine(a2,b2,c2,d2)) ->  Combine( a1,b1@ (rev a2) @ b2 ,c1,d2) ;;



let reverse myString = match myString with
			EmptyString -> EmptyString
			| Alphabet (a)-> Alphabet (a)
			| Combine (a,b,c,d) -> Combine(b,a,d,c);;

let first myString = match myString with
| EmptyString -> raise Empty
| Alphabet(a) -> a
| Combine(a,b,c,d) -> c;;

let last myString = match myString with
| EmptyString -> raise Empty
| Alphabet(a) -> a
| Combine(a,b,c,d) -> d;;

(* let create String = ; *)

let forward myString = match myString with	
						EmptyString -> raise AtLast
						| Alphabet(a) -> raise AtLast
						| Combine(a,[],c,d) -> raise AtLast 
						| Combine(a,x::xs,c,d) -> Combine([x]@a,xs,c,d);;

let rec forward2 (a1,n) = match (a1,n) with	
						(EmptyString,_) -> raise AtLast
						| (Alphabet(a),_) -> raise AtLast
						| (Combine(a,[],c,d),_) -> raise AtLast 
						| (Combine(a,x::xs,c,d),1) -> Combine([x]@a,xs,c,d)
						| (Combine(a,x::xs,c,d),n) -> forward2 ( Combine([x]@a,xs,c,d) , n -1 );;


let back myString = match myString with	
						EmptyString -> raise AtFirst
						| Alphabet(a) -> raise AtFirst
						| Combine([x],b,c,d) -> raise AtFirst 
						| Combine([],b,c,d) -> raise Error 
						| Combine(x::xs,b,c,d) -> Combine(xs,[x]@b,c,d);;

let rec back2 (myString,n) = match (myString,n) with	
						(EmptyString,_) -> raise AtFirst
						| (Alphabet(a),_) -> raise AtFirst
						| (Combine([x],b,c,d),_) -> raise AtFirst 
						| (Combine([],b,c,d),_) -> raise Error 
						| (Combine(x::xs,b,c,d),1) -> Combine(xs,[x]@b,c,d)
						| (Combine(x::xs,b,c,d),_) -> back2 (Combine(xs,[x]@b,c,d),n-1);;


let moveTo myString n s = match (n,s) with 
						(_,EmptyString) -> raise Empty
						| (0,Alphabet(a)) -> Alphabet(a)
						| (_,Alphabet(a)) -> raise TooShort
						| (n,Combine(a,b,c,d)) -> 	if((lgh(Combine(a,b,c,d))) > n) 
														then (		if ((length a)-1) = n 
																		then Combine(a,b,c,d) 
																	else if ((length a)-1) < n   then
																		forward2 (Combine(a,b,c,d),(n - length a) + 1) 
																	else	
																		back2 (Combine(a,b,c,d), (n - length a) + 1)
																)
													else raise TooShort;;