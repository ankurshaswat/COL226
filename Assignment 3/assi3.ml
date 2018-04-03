open List;;
exception Error;;
exception NOT_UNIFIABLE;;

type variable = Var of string;;

(* Defining variables *)
let x=Var("x");;
let y=Var("y");;
let z=Var("z");;

type symbol = Sym of string;;

(* Defining Symbols *)
let plus=Sym("+");;
let minus=Sym("-");;
let one=Sym("1");;
let two=Sym("2");;

type arity = Ar of int;;

(* Defining arity placeholders. *)
let ar_zero=Ar(0);;
let ar_one=Ar(1);;
let ar_two=Ar(2);;

type term = V of variable | Node of symbol * (term list);;

(* Defining terms *)
let termx = V(x);;
let termy = V(y);;
let term1 = Node(one,[]);;
let term2 = Node(two,[]);;
let termxx = Node((plus,[term1;termy]));;
let termxx2 = Node((plus,[term1;termx]));;
let termyy = Node(plus,[term2;termx]);;
let termzz = Node(plus,[termxx;termyy]);;

type signature = Sig of (symbol * arity) list;;

(* Defining a signature using above terms and symbols. *)
let sig1 = Sig([plus,ar_two;minus,ar_two;one,ar_zero;two,ar_zero]);;

let rec not_exist_in_list target l = match (l,target) with
  | (Sig([]),Sym(tar)) -> true
  | (Sig((Sym(sym), Ar(n))::xs),Sym(tar)) -> if sym = tar then false else (not_exist_in_list (Sym(tar)) (Sig(xs)))

let rec not_exist_in_list2 target l = match (l,target) with
  | ([],s) -> true
  | (Var(a)::xs,s) -> if a = s then false else (not_exist_in_list2 s xs);;

let rec check_sig sign = match sign with
  | Sig([(Sym(sym),Ar(n))]) -> if(n<0) then false else true
  | Sig((Sym(sym),Ar(n))::xs) -> check_sig(Sig([(Sym(sym),Ar(n))])) && (not_exist_in_list (Sym(sym))  (Sig(xs))) && check_sig(Sig(xs))
  | Sig([]) -> true;;

(* Checking Signture for correctness *)
check_sig(sig1);;

let ander a b = a && b;;

let sum a b= a+b;;

let rec add_to_list a b= match (a,b) with
  | ([],b) -> b
  | (a,[]) -> a
  | (Var(x)::xs,b) -> if (not_exist_in_list2 x b) then add_to_list xs (Var(x)::b) else add_to_list xs b;;

let rec foldl f e l = match l with
  | [] -> e
  | x::xs -> f x (foldl f e xs);;

let rec map f l = match l with
  | []-> []
  | x::xs -> (f x)::(map f xs);;

let rec get_arity target l = match (l,target) with
  | (Sig([]),Sym(tar)) -> raise Error
  | (Sig((Sym(sym), Ar(n))::xs),Sym(tar)) -> if sym = tar then Ar(n) else (get_arity (Sym(tar)) (Sig(xs)));;

let rec wfterm sign term= match term with
  | V(var) -> true
  | Node(sym,tl) -> ((get_arity sym sign) = Ar(length(tl))) && foldl ander true (map (wfterm sign) tl);;

(* Checking the terms for correctness with respect to current signature. *)
wfterm sig1 termx;;
wfterm sig1 termy;;
wfterm sig1 term1;;
wfterm sig1 term2;;
wfterm sig1 termxx;;
wfterm sig1 termyy;;
wfterm sig1 termzz;;

let rec ht term = match term with
  | V(var) -> 0
  | Node(sym,[]) -> 0
  | Node(sym,tl) -> 1 + (foldl max 0 (map ht tl));;

(* Checking height *)
ht(termzz);;

let rec size term = match term with
  | V(var) -> 1
  | Node(sym,[]) -> 1
  | Node(sym,tl) -> 1 + (foldl sum 0 (map size tl));;

(* Checking size *)
size(termzz);;

let rec vars term = match term with
  | V(var) -> [var]
  | Node(sym,tl) -> foldl add_to_list [] (map vars tl);;

(* Checking vars function *)
vars(termzz);;

let rec find a lis = match lis with
  | x::xs -> if(a=x) then true else (find a xs)
  | [] -> false;;

type substituion = Sub of (variable * term) list;;

(* Defining 1 substion *)
let sub_test = Sub([ x,Node(plus,[V(y);V(z)])]);;

let rec subst subst1 t  = match (t,subst1) with
  | (V(Var(s)),Sub(x)) -> (try  List.assoc (Var(s)) x with | Not_found -> V(Var(s)))
  | (Node(x,y),Sub(z)) -> Node(x,map (subst subst1) y)
;;

let rec map2 f l = match l with
  | []-> []
  | ((a,b)::xs) -> (a,(f b))::(map2 f xs);;


let rec find2 a lis = match lis with
  | (x,y)::xs -> if(a=x) then true else (find2 a xs)
  | [] -> false;;

let rec get_non_repeated listA listB = match (listA,listB) with
  | (Sub((v,tL)::xs),Sub(b)) -> if (find2 v b) then  (get_non_repeated (Sub(xs)) listB) else [v,tL]@(get_non_repeated (Sub(xs)) listB)
  | (Sub([]),Sub(b)) -> [];;

let compose subst1 subst2 = match (subst1,subst2) with
  | (Sub(x),Sub(y)) -> Sub((map2 (subst subst2) x) @ (get_non_repeated subst2 subst1)) ;;

(* Substituting in term using the substituion defined above *)
termzz;;
let term_final=(subst sub_test termzz);;
wfterm sig1 term_final;;

let rec mgu t1 t2 = match (t1,t2) with
  | (V(Var(s)),V(Var(t))) -> if (s=t) then Sub([]) else Sub([(Var(s),V(Var(t)))])
  | (V(Var(s)),Node(Sym(sym),[])) ->  Sub([(Var(s),Node(Sym(sym),[]))])
  | (Node(Sym(sym),[]),V(Var(s))) ->  Sub([(Var(s),Node(Sym(sym),[]))])
  | (V(Var(s)),Node(Sym(sym),tl)) -> if (find (Var(s)) (vars(Node(Sym(sym),tl)))) then raise NOT_UNIFIABLE else Sub([(Var(s),Node(Sym(sym),tl))])
  | (Node(Sym(sym),tl),V(Var(s))) -> if (find (Var(s)) (vars(Node(Sym(sym),tl)))) then raise NOT_UNIFIABLE else Sub([(Var(s),Node(Sym(sym),tl))])
  | (Node(Sym(sym1),[]),Node(Sym(sym2),[])) ->  if (sym1=sym2) then Sub([]) else raise NOT_UNIFIABLE
  | (Node(Sym(sym1),[]),Node(Sym(sym2),tl)) ->  raise NOT_UNIFIABLE
  | (Node(Sym(sym2),tl),Node(Sym(sym1),[])) ->  raise NOT_UNIFIABLE
  | (Node(Sym(sym1),x::xs),Node(Sym(sym2),y::ys)) ->  if (sym1<>sym2) then raise NOT_UNIFIABLE else compose (mgu x y) (mgu (Node(Sym(sym1),(map (subst (mgu x y)) xs))) (Node(Sym(sym1),(map (subst (mgu x y)) ys))))   ;;

(* Checking the mgu for the obtained term . To be correct it should be same as the above defined substituion *)
mgu termzz term_final;;
(* Checking the mgu from 2 terms *)
mgu termxx2 termxx;;
(* Checking mgu for non unifiable terms *)
mgu termxx termyy;;
