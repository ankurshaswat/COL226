exception Empty;;
exception AtLast;;
exception AtFirst;;
exception TooShort;;
exception Error;;
open List;;
open String;;

type 'a myString = {mutable a:'a list;mutable b:'a list;mutable c:'a;mutable d:'a};;


(* val rev : 'a list -> 'a list = <fun>   Helping Function*)
let rec rev a = match a with
  |  [] ->  []
  | x::xs -> (rev xs) @ [x];;

(* val lgh : 'a myString -> int = <fun> *)
let lgh myString = (List.length myString.a)+(List.length myString.b);;

(* val nonempty : 'a myString -> bool = <fun> *)
let nonempty myString = match myString with
  |  {a=[];b=[];c;d} -> false
  | _ -> true;;

(* val concat : 'a myString -> 'a myString -> 'a myString = <fun> *)
let concat s1 s2 = {a=s1.a;b=s1.b @ (rev s2.a) @ s2.b;c=s1.c;d=s2.d};;

(* val reverse : 'a myString -> 'a myString = <fun> *)
let reverse myString =  match myString with
  | {a=a1;b=b1;c=c1;d=d1} ->  myString.a <-b1 ; myString.b <- a1 ; myString.c <- d1 ; myString.d <-c1;myString;;

(* val first : 'a myString -> 'a = <fun> *)
let first myString = match myString with
  | {a=[];b=[];c;d} -> raise Empty
  | {a=a1;b=b1;c=c1;d=d1} -> c1;;

(* val last : 'a myString -> 'a = <fun> *)
let last myString = match myString with
  | {a=[];b=[];c;d} -> raise Empty
  | {a=a1;b=b1;c=c1;d=d1} -> d1;;

(* val stringToList : string -> char list = <fun>     Helping Function*)
let rec stringToList string = match string with
  | "" -> []
  | string -> (String.get string 0) :: stringToList (String.sub string 1 ((String.length string)-1));;

(* val create : string -> char myString = <fun> *)
let create string = match string with
  | "" -> {a=[];b=[];c=String.get "" 0;d=String.get "" 0}
  | string -> if (String.length string > 1)
    then {a=[String.get string 0];b=stringToList (String.sub string 1 ((String.length string)-1));c=String.get string 0;d=String.get string ((String.length string)- 1)}
    else {a=[String.get string 0];b=[];c=String.get string 0;d=String.get string 0}  ;;

(* val forward : 'a myString -> 'a myString = <fun> *)
let forward myString = match myString with
  | {a=[];b=[];c;d} -> raise Empty
  | {a=a1;b=[];c=c1;d=d1} -> raise AtLast
  | {a=a1;b=x::xs;c=c1;d=d1} ->  myString.a<- [x]@a1;myString.b<-xs;myString;;

(* val forward2 : 'a myString * int -> 'a myString = <fun>  Helping Function*)
let rec forward2 (myString,n) = match (myString,n) with
  | ({a=[];b=[];c;d},n) -> raise AtLast
  | ({a=a1;b=[];c=c1;d=d1},0) -> myString.a<-a1;myString.b<-[];myString
  | ({a=a1;b=x::xs;c=c1;d=d1} ,1) -> myString.a<-[x]@a1;myString.b<-xs;myString
  | ({a=a1;b=x::xs;c=c1;d=d1} ,n) -> myString.a<-[x]@a1;myString.b<-xs;forward2 (myString,n-1)
  | ({a=_::_;b=[];c=_;d=_},n) -> raise AtLast;;

(* val back : 'a myString -> 'a myString = <fun> *)
let back myString = match myString with
  | {a=[];b=[];c;d} -> raise Empty
  | {a=[];b=b1;c=c1;d=d1} -> raise AtFirst
  | {a=x::xs;b=b1;c=c1;d=d1} ->  myString.a<- xs;myString.b<-[x]@b1;myString;;

(* val back2 : 'a myString * int -> 'a myString = <fun>   Helping Function*)
let rec back2 (myString,n) = match (myString,n) with
  | ({a=[];b=[];c;d},n) -> raise AtFirst
  | ({a=[x];b=b1;c=c1;d=d1},0) -> myString.a<-[x];myString.b<-b1;myString
  | ({a=[x];b=b1;c=c1;d=d1},n) ->raise AtFirst
  | ({a=x::xs;b=b1;c=c1;d=d1} ,n) -> myString.a<-xs;myString.b<-x::b1;back2 (myString,n-1)
  | ({a=[];b=_::_;c=_;d=_},n) -> raise Error;;

(* val moveTo : int -> 'a myString -> 'a myString = <fun> *)
let moveTo n myString = match (n,myString) with
  | (_,{a=[];b=[];c;d}) -> raise Empty
  | (n,{a=a1;b=b1;c=c1;d=d1}) ->   if((lgh(myString)) > n)
    then (
      if ((List.length a1)-1) = n
      then (myString.a<-a1;myString.b<-b1;myString)
      else if ((List.length a1)-1) < n   then forward2 (myString,(n - List.length a1) + 1)
      else back2 (myString,(n - List.length a1) + 1)
    )
    else raise TooShort;;

(* val replace : 'a -> 'a myString -> 'a myString = <fun> *)
let replace w myString = match (w,myString) with
  | (w,{a=[];b=[];c;d}) -> raise Empty
  | (w,{a=[x];b=b1;c=c1;d=d1}) -> myString.a<-[w];myString.c<-w;myString
  | (w,{a=x::xs;b=[];c=c1;d=d1}) -> myString.a<-w::xs;myString.d<-w;myString
  | (w,{a=x::xs;b=b1;c=c1;d=d1}) -> myString.a<-w::xs;myString
  | (w,{a=[];b=b1;c=c1;d=d1}) -> raise Error;;

(* val printList : char list -> string = <fun>  Testing Function For char myString*)
let rec printList l = match l with
  | [] -> ""
  | x::xs ->  (String.make 1 x)^(printList xs);;

(* val print : char myString -> string = <fun>  Testing Function For char myString *)
let print myString =  printList((rev myString.a)@myString.b) ;;
