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

let rec check_sig sign = match sign with
  | Sig([(Sym(sym),Ar(n))]) -> if(n<0) then false else true
  | Sig((Sym(sym),Ar(n))::xs) -> check_sig(Sig([(Sym(sym),Ar(n))])) && (not_exist_in_list (Sym(sym))  (Sig(xs))) && check_sig(Sig(xs))
  | Sig([]) -> true;;

  (* Checking Signture for correctness *)
check_sig(sig1);;

let ander a b = a && b;;

let sum a b= a+b;;

let add_to_list a b= a@b;;

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

let rec find2 a lis = match lis with
  | (x,y)::xs -> if(a=x) then true else (find2 a xs)
  | [] -> false;;

type substss = Substituon1 of (variable * term);;

(* Defining 1 substion *)
let substituions = [ Substituon1(x,Node(plus,[V(y);V(z)])) ];;

(* Each substituion is an element of the above list. Composition of substituions is a complete list in which the second substituion is appended to the first element. *)

let rec applySubst subst t = match (t,subst) with
  | (V(Var(s)),Substituon1(Var(t2),a)) -> if (s=t2) then a else V(Var(s))
  | (Node(Sym(s),tl),_) -> Node(Sym(s), map (applySubst subst) tl);;

let rec subst substList t  = match (substList,t) with
  | (x::xs,t1) -> subst xs (applySubst x t1)
  | ([],t1) -> t1;;

(* Substituting in term using the substituion present in the list defined above *)
termzz;;
let term_final=(subst substituions termzz);;
wfterm sig1 term_final;;

let rec mgu t1 t2 = match (t1,t2) with
  | (V(Var(s)),V(Var(t))) -> if (s=t) then [] else [Substituon1(Var(s),V(Var(t)))]
  | (V(Var(s)),Node(Sym(sym),[])) ->  [Substituon1(Var(s),Node(Sym(sym),[]))]
  | (Node(Sym(sym),[]),V(Var(s))) ->  [Substituon1(Var(s),Node(Sym(sym),[]))]
  | (V(Var(s)),Node(Sym(sym),tl)) -> if (find (Var(s)) (vars(Node(Sym(sym),tl)))) then raise NOT_UNIFIABLE else [Substituon1(Var(s),Node(Sym(sym),tl))]
  | (Node(Sym(sym),tl),V(Var(s))) -> if (find (Var(s)) (vars(Node(Sym(sym),tl)))) then raise NOT_UNIFIABLE else [Substituon1(Var(s),Node(Sym(sym),tl))]
  | (Node(Sym(sym1),[]),Node(Sym(sym2),[])) ->  if (sym1=sym2) then [] else raise NOT_UNIFIABLE
  | (Node(Sym(sym1),[]),Node(Sym(sym2),tl)) ->  raise NOT_UNIFIABLE
  | (Node(Sym(sym2),tl),Node(Sym(sym1),[])) ->  raise NOT_UNIFIABLE
  | (Node(Sym(sym1),x::xs),Node(Sym(sym2),y::ys)) ->  if (sym1<>sym2) then raise NOT_UNIFIABLE else (mgu x y)@ (mgu (Node(Sym(sym1),(map (subst (mgu x y)) xs))) (Node(Sym(sym1),(map (subst (mgu x y)) ys))))   ;;

(* Checking the mgu for the obtained term . To be correct it should be same as the above defined substituion *)
mgu termzz term_final;;
(* Checking the mgu from 2 terms *)
mgu termxx2 termxx;;
(* Checking mgu for non unifiable terms *)
mgu termxx termyy;;
