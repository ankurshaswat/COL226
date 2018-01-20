exception Empty;;
exception AtLast;;
exception AtFirst;;
exception TooShort;;
exception Error;;
open List;;
open String;;

type 'a myString = EmptyString | Alphabet of 'a | Combine of ('a list * 'a list * 'a * 'a);;

let rec rev a = match a with
  |  [] ->  []
  | x::xs -> (rev xs) @ [x];;

let lgh myString = match myString with
  |  EmptyString -> 0
  |  Alphabet (a)-> 1
  |  Combine (a,b,c,d) -> (List.length a) + (List.length b);;


let nonempty myString = match myString with
  |  EmptyString -> false
  | _ -> true;;


let concat a b = match (a,b) with
  |  (a,EmptyString) -> a
  | (EmptyString,b) -> b
  |  (Alphabet(e1),Alphabet(e2)) -> Combine ([e1],[e2],e1,e2)
  | (Alphabet(e1),Combine(a1,b1,c1,d1)) ->  Combine(a1@[e1],b1,e1,d1)
  | (Combine(a1,b1,c1,d1),Alphabet(e1)) ->  Combine(a1,b1@[e1],c1,e1)
  | (Combine(a1,b1,c1,d1),Combine(a2,b2,c2,d2)) ->  Combine( a1,b1@ (rev a2) @ b2 ,c1,d2) ;;

let reverse myString = match myString with
  |  EmptyString -> EmptyString
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

let rec stringToList string = match string with
  | "" -> []
  | string -> (String.get string 0) :: stringToList (String.sub string 1 ((String.length string)-1));;

let create string = match string with
  | "" -> EmptyString
  | string -> if (String.length string > 1) then Combine([String.get string 0],stringToList (String.sub string 1 ((String.length string)-1)),String.get string 0,String.get string ((String.length string)- 1))   else Alphabet(String.get string 0);;

let forward myString = match myString with
  | EmptyString -> raise AtLast
  | Alphabet(a) -> raise AtLast
  | Combine(a,[],c,d) -> raise AtLast
  | Combine(a,x::xs,c,d) -> Combine([x]@a,xs,c,d);;

let rec forward2 (a1,n) = match (a1,n) with
  |  (EmptyString,_) -> raise AtLast
  | (Alphabet(a),_) -> raise AtLast
  | (Combine(a,[],c,d),_) -> raise AtLast
  | (Combine(a,x::xs,c,d),1) -> Combine([x]@a,xs,c,d)
  | (Combine(a,x::xs,c,d),n) -> forward2 ( Combine([x]@a,xs,c,d) , n -1 );;


let back myString = match myString with
  |  EmptyString -> raise AtFirst
  | Alphabet(a) -> raise AtFirst
  | Combine([x],b,c,d) -> raise AtFirst
  | Combine([],b,c,d) -> raise Error
  | Combine(x::xs,b,c,d) -> Combine(xs,[x]@b,c,d);;

let rec back2 (myString,n) = match (myString,n) with
  |  (EmptyString,_) -> raise AtFirst
  | (Alphabet(a),_) -> raise AtFirst
  | (Combine([x],b,c,d),_) -> raise AtFirst
  | (Combine([],b,c,d),_) -> raise Error
  | (Combine(x::xs,b,c,d),1) -> Combine(xs,[x]@b,c,d)
  | (Combine(x::xs,b,c,d),_) -> back2 (Combine(xs,[x]@b,c,d),n-1);;


let moveTo n s = match (n,s) with
  |  (_,EmptyString) -> raise Empty
  | (0,Alphabet(a)) -> Alphabet(a)
  | (_,Alphabet(a)) -> raise TooShort
  | (n,Combine(a,b,c,d)) -> 	if((lgh(Combine(a,b,c,d))) > n)
    then (		if ((List.length a)-1) = n
            then Combine(a,b,c,d)
            else if ((List.length a)-1) < n   then
              forward2 (Combine(a,b,c,d),(n - List.length a) + 1)
            else
              back2 (Combine(a,b,c,d), (n - List.length a) + 1)
         )
    else raise TooShort;;

let replace w s = match (w,s) with
  | (_,EmptyString) -> raise Empty
  | (w,Alphabet(a)) -> Alphabet(w)
  | (w,Combine([x],b,c,d)) -> Combine([w],b,w,d)
  | (w,Combine(x::xs,[],c,d)) -> Combine([w]@xs,[],c,w)
  | (w,Combine(x::xs,b,c,d)) ->  Combine([w]@xs,b,c,d)
  | (w,Combine([],b,c,d)) -> raise Error;;

let rec printList l = match l with
  | [] -> ""
  | x::xs ->  (String.make 1 x)^(printList xs);;

let print myString = match myString with
  | EmptyString -> ""
  | Alphabet(a) -> (String.make 1 a)
  | Combine(a,b,c,d) -> printList((rev a)@b) ;;
