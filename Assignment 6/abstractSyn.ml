exception ErrorUnify;;
exception NoPossibleSolution;;
exception NOT_UNIFIABLE;;
exception Error;;
exception False;;
exception Error1;;
exception Error2;;
exception ErrorSubAF;;
open List;;
exception ErrorSubList;;
type variable = Var of string;;

type term = V of variable | Function of string * (term list);;

type substituion = Sub of (variable * term) list;;

type atomic_formula = term;;

type clause = Fact of atomic_formula
            | Rule of atomic_formula * (atomic_formula list) ;;

type program = clause list;;

type goal = atomic_formula list;;

let rec map f l = match l with
  | []-> []
  | x::xs -> (f x)::(map f xs);;

let rec mapFunc l t= match l with
  | []-> []
  | x::xs -> (x t)::(mapFunc xs t);;


let rec invertmap f lvals term = match lvals with
  | []-> []
  | x::xs -> (f x term)::(invertmap f xs term);;

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
  | (Sub((v,tL)::xs),Sub(b)) -> let tempVar = (get_non_repeated (Sub(xs)) listB) in (if (find2 v b) then tempVar  else [v,tL]@tempVar)
  | (Sub([]),Sub(b)) -> [];;

let rec substitute subst1 t  = match (t,subst1) with
  | (V(Var(s)),Sub(x)) -> (try  List.assoc (Var(s)) x with | Not_found -> V(Var(s)))
  | (Function(x,y),Sub(z)) -> Function(x,map (substitute subst1) y);;

let rec transformSingle depth subst1 t = match (t,subst1) with
  | (V(Var(s)),Sub(x)) -> (try  (List.assoc (Var(s)) x) with | Not_found -> V(Var((string_of_int (depth)) ^ s)))
  | (Function(x,y),Sub(z)) -> Function(x,map (transformSingle depth subst1) y);;

let rec transform depth t = match t with
  | V(Var(s)) ->  V(Var((string_of_int (depth)) ^ s))
  | Function(x,y) -> Function(x,map (transform depth) y);;

let compose subst1 subst2 = match (subst1,subst2) with
  | (Sub(x),Sub(y)) -> Sub((map2 (substitute subst2) x) @ (get_non_repeated subst2 subst1)) ;;

let rec find a lis = match lis with
  | x::xs -> if(a=x) then true else (find a xs)
  | [] -> false;;

let rec mgu t1 t2 = match (t1,t2) with
  | (V(Var(s)),V(Var(t))) -> if (s=t) then Sub([]) else Sub([(Var(t),V(Var(s)))])
  | (V(Var(s)),Function((sym),[])) ->  Sub([(Var(s),Function((sym),[]))])
  | (Function((sym),[]),V(Var(s))) ->  Sub([(Var(s),Function((sym),[]))])
  | (V(Var(s)),Function((sym),tl)) -> if (find (Var(s)) (vars(Function((sym),tl)))) then raise NOT_UNIFIABLE else Sub([(Var(s),Function((sym),tl))])
  | (Function((sym),tl),V(Var(s))) -> if (find (Var(s)) (vars(Function((sym),tl)))) then raise NOT_UNIFIABLE else Sub([(Var(s),Function((sym),tl))])
  | (Function((sym1),[]),Function((sym2),[])) ->  if (sym1=sym2) then Sub([]) else raise NOT_UNIFIABLE
  | (Function((sym1),[]),Function((sym2),tl)) ->  raise NOT_UNIFIABLE
  | (Function((sym2),tl),Function((sym1),[])) ->  raise NOT_UNIFIABLE
  | (Function((sym1),x::xs),Function((sym2),y::ys)) ->  if (sym1<>sym2) then raise NOT_UNIFIABLE else (let unifierVar = (mgu x y) in (let mapperVar = map (substitute unifierVar) in compose unifierVar (mgu (Function((sym1),(mapperVar xs))) (Function((sym1),(mapperVar ys))))));;

let rec composer l1 ll2 =match (l1,ll2) with
  | (x::xs,y::ys) ->  (match y with
      | [] -> (composer xs ys)
      | _ -> ((map (compose x)) y)::(composer xs ys)
    )
  | ([],[]) -> []
  | ([],y::ys) -> raise Error1
  | (ll,[]) -> raise Error2;;

let rec mix l =match l with
  | [] -> []
  | x::[] -> x
  | x::y::xs -> if(x=y) then x@(mix xs) else x@(mix (y::xs));;

let substituteAF subst1 t  = match t with
  | Function(x,y) ->Function(x,map (substitute subst1) y)
  | _ -> raise ErrorSubAF;;

let rec substituteList substList termList = match (termList,substList) with
  | (termList,x::[]) -> [map (substitute x) termList]
  | (termList,[]) -> []
  | (termList,x::xs) -> (substituteList [x] termList)@(substituteList xs termList);;

let rec check ff afl = match (ff,afl) with
  | (ff,x::xs) -> if (ff=x) then raise NOT_UNIFIABLE else x::(check ff xs)
  | (_,[]) -> [];;

let rec unify depth goal program completeProg= match (goal,program) with
  | ([],ll) -> [Sub[]]
  | (x::[],Fact(Function(s,tl))::xs) -> let smallTempVar = (unify depth [x] xs completeProg)  in
    (try (mgu x (transform (depth+1) (Function(s,tl))))::smallTempVar with | NOT_UNIFIABLE -> smallTempVar)
  | (ll,[]) -> []
  | (x::[],Rule(Function(s,tl),afl)::xs) ->
    let transformedFunc = (transform (depth+1) (Function(s,tl))) in
    (let anotherTempVar= (unify depth ([x]) xs completeProg) in
     (try  (let tempVar = (mgu x transformedFunc) in
            (map (compose tempVar) (unify (depth+1) (check (substitute tempVar transformedFunc) (map (substituteAF tempVar) (map (transform (depth+1)) afl))) completeProg completeProg)))@anotherTempVar
      with | NOT_UNIFIABLE -> anotherTempVar))

  | (x::xs,program) -> (match (unify depth [x] program completeProg) with
      | [] -> []
      | l -> mix (composer l (mapFunc(mapFunc((map (unify depth)) (substituteList l xs)) program) completeProg))
    );;

let rec subster goalList subLis = match (goalList,subLis) with
  | (goalList,x::[]) -> [map (substitute x) goalList]
  | (goalList,x::xs) -> [map (substitute x) goalList]@(subster goalList xs)
  | (_,[]) -> raise False;;

(* let prolog goals subLis = match subLis with
   | [] -> raise False
   | _ -> subster goals subLis;; *)

let rec removeDup goalList = match (goalList) with
  | []-> []
  | (x::xs) -> let unduplicatedList = (removeDup xs) in (if (find x unduplicatedList) then unduplicatedList else x::unduplicatedList);;

let run_prolog goalList program = removeDup (subster goalList (unify 0 goalList program program));;
