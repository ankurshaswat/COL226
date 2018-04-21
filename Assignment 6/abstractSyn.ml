exception Error;;
exception NOT_UNIFIABLE;;

type variable = Var of string;;

type term = V of variable | Function of string * (term list);;

type substituion = Sub of (variable * term) list;;

type atomic_formula = Function of string * (term list);;

type clause = Fact of atomic_formula
            | Rule of atomic_formula * (atomic_formula list) ;;

type program = clause list;;

type goal = atomic_formula list;;

let rec map f l = match l with
  | []-> []
  | x::xs -> (f x)::(map f xs);;

let rec not_exist_in_list2 target l = match (l,target) with
  | ([],s) -> true
  | (Var(a)::xs,s) -> if a = s then false else (not_exist_in_list2 s xs);;

let rec add_to_list a b= match (a,b) with
  | ([],b) -> b
  | (a,[]) -> a
  | (Var(x)::xs,b) -> if (not_exist_in_list2 x b) then add_to_list xs (Var(x)::b) else add_to_list xs b;;

let rec foldl f e l = match l with
  | [] -> e
  | x::xs -> f x (foldl f e xs);;

let rec vars term = match term with
  | V(var) -> [var]
  | Function(sym,tl) -> foldl add_to_list [] (map vars tl);;

let rec map2 f l = match l with
  | []-> []
  | ((a,b)::xs) -> (a,(f b))::(map2 f xs);;

let rec find2 a lis = match lis with
  | (x,y)::xs -> if(a=x) then true else (find2 a xs)
  | [] -> false;;

let rec get_non_repeated listA listB = match (listA,listB) with
  | (Sub((v,tL)::xs),Sub(b)) -> if (find2 v b) then  (get_non_repeated (Sub(xs)) listB) else [v,tL]@(get_non_repeated (Sub(xs)) listB)
  | (Sub([]),Sub(b)) -> [];;

let rec substitute subst1 t  = match (t,subst1) with
  | (V(Var(s)),Sub(x)) -> (try  List.assoc (Var(s)) x with | Not_found -> V(Var(s)))
  | (Function(x,y),Sub(z)) -> Function(x,map (substitute subst1) y);;

let compose subst1 subst2 = match (subst1,subst2) with
  | (Sub(x),Sub(y)) -> Sub((map2 (substitute subst2) x) @ (get_non_repeated subst2 subst1)) ;;

let rec find a lis = match lis with
  | x::xs -> if(a=x) then true else (find a xs)
  | [] -> false;;

let rec mgu t1 t2 = match (t1,t2) with
  | (V(Var(s)),V(Var(t))) -> if (s=t) then Sub([]) else Sub([(Var(s),V(Var(t)))])
  | (V(Var(s)),Function((sym),[])) ->  Sub([(Var(s),Function((sym),[]))])
  | (Function((sym),[]),V(Var(s))) ->  Sub([(Var(s),Function((sym),[]))])
  | (V(Var(s)),Function((sym),tl)) -> if (find (Var(s)) (vars(Function((sym),tl)))) then raise NOT_UNIFIABLE else Sub([(Var(s),Function((sym),tl))])
  | (Function((sym),tl),V(Var(s))) -> if (find (Var(s)) (vars(Function((sym),tl)))) then raise NOT_UNIFIABLE else Sub([(Var(s),Function((sym),tl))])
  | (Function((sym1),[]),Function((sym2),[])) ->  if (sym1=sym2) then Sub([]) else raise NOT_UNIFIABLE
  | (Function((sym1),[]),Function((sym2),tl)) ->  raise NOT_UNIFIABLE
  | (Function((sym2),tl),Function((sym1),[])) ->  raise NOT_UNIFIABLE
  | (Function((sym1),x::xs),Function((sym2),y::ys)) ->  if (sym1<>sym2) then raise NOT_UNIFIABLE else compose (mgu x y) (mgu (Function((sym1),(map (substitute (mgu x y)) xs))) (Function((sym1),(map (substitute (mgu x y)) ys))));;

let rec unifyClause atomic_formula clause = match (atomic_formula,clause) with
  | (Function(s1,tl1),Fact(Function(s2,tl2))) -> mgu (Function(s1,tl1)) (Function(s2,tl2))
  (* | (Function(s1,tl1),Rule(af1,afl) -> *)
  | (_,_) -> raise Error;;

let rec unifyAtomic atomic_formula program = match (atomic_formula,program) with
  | (atomic_formula,x::[]) -> [unifyClause atomic_formula x]
  | (atomic_formula,x::xs) -> (try [unifyClause atomic_formula x]@(unifyAtomic atomic_formula xs)
                               with
                               | NOT_UNIFIABLE -> unifyAtomic atomic_formula xs
                               | _ -> failwith "Unknown")
  | (_,_) -> raise Error;;

let rec unify goal program = match (goal,program) with
  | (x::[],program) -> unifyAtomic x program
  (* | (x::xs,program) -> (unify ([x]) program) (unify xs program) *)
  | (_,_) -> raise Error;;
