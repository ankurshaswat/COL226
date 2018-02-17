open List;;
exception Error;;
exception NOT_UNIFIABLE;;

type variable = Var of string;;

type symbol = Sym of string;;

type arity = Ar of int;;

type term = V of variable | Node of symbol * (term list);;

type signature = Sig of (symbol * arity) list;;

let rec not_exist_in_list target l = match (l,target) with
  | (Sig([]),Sym(tar)) -> true
  | (Sig((Sym(sym), Ar(n))::xs),Sym(tar)) -> if sym = tar then false else (not_exist_in_list (Sym(tar)) (Sig(xs)))

let rec check_sig sign = match sign with
  | Sig([(Sym(sym),Ar(n))]) -> if(n<0) then false else true
  | Sig((Sym(sym),Ar(n))::xs) -> check_sig(Sig([(Sym(sym),Ar(n))])) && (not_exist_in_list (Sym(sym))  (Sig(xs))) && check_sig(Sig(xs))
  | Sig([]) -> true;;

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

let rec ht term = match term with
  | V(var) -> 0
  | Node(sym,[]) -> 0
  | Node(sym,tl) -> 1 + (foldl max 0 (map ht tl));;

let rec size term = match term with
  | V(var) -> 1
  | Node(sym,[]) -> 1
  | Node(sym,tl) -> 1 + (foldl sum 0 (map size tl));;

let rec vars term = match term with
  | V(var) -> [var]
  | Node(sym,tl) -> foldl add_to_list [] (map vars tl);;

let rec find a lis = match lis with
  | x::xs -> if(a=x) then true else (find a xs)
  | [] -> false;;

let rec find2 a lis = match lis with
  | (x,y)::xs -> if(a=x) then true else (find2 a xs)
  | [] -> false;;

let substituions =[ Var("x"),Node(Sym("+"),[V(Var("y"));V(Var("z"))]) ];;

(* Each substituion is an element of the above list. Composition of substituions is a complete list in which the second substituion is appended to the first element. *)

let rec subst substTable t  = match t with
  | V(Var(s)) -> if (find2 (Var(s)) substTable) then (List.assoc (Var(s)) substTable) else V(Var(s))
  | Node(Sym(s),tl) -> Node(Sym(s),(map (subst substTable) tl));;

let rec mgu t1 t2 = match (t1,t2) with
  | (V(Var(s)),V(Var(t))) -> if (s=t) then [] else [Var(s),V(Var(t))]
  | (V(Var(s)),Node(Sym(sym),[])) ->  [Var(s),Node(Sym(sym),[])]
  | (Node(Sym(sym),[]),V(Var(s))) ->  [Var(s),Node(Sym(sym),[])]
  | (V(Var(s)),Node(Sym(sym),tl)) -> if (find (Var(s)) (vars(Node(Sym(sym),tl)))) then raise NOT_UNIFIABLE else [Var(s),Node(Sym(sym),tl)]
  | (Node(Sym(sym),tl),V(Var(s))) -> if (find (Var(s)) (vars(Node(Sym(sym),tl)))) then raise NOT_UNIFIABLE else [Var(s),Node(Sym(sym),tl)]
  | (Node(Sym(sym1),[]),Node(Sym(sym2),[])) ->  if (sym1=sym2) then [] else raise NOT_UNIFIABLE
  | (Node(Sym(sym1),[]),Node(Sym(sym2),tl)) ->  raise NOT_UNIFIABLE
  | (Node(Sym(sym2),tl),Node(Sym(sym1),[])) ->  raise NOT_UNIFIABLE
  | (Node(Sym(sym1),x::xs),Node(Sym(sym2),y::ys)) ->  if (sym1<>sym2) then raise NOT_UNIFIABLE else mgu (Node(Sym(sym1),(map (subst (mgu x y)) xs))) (Node(Sym(sym1),(map (subst (mgu x y)) ys)))   ;;
