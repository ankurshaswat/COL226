type term = Var of string | Const of int | Function of string * (term list);;

type atomic_formula = Function of string * (term list);;

type clause = Fact of atomic_formula
            | Rule of atomic_formula * (atomic_formula list) ;;

type program = clause list;;

type goal = atomic_formula list;;

let rec unifyAF af head = match (af,head) with
| (AtomicFormula(s1,tl1),AtomicFormula(s2,tl2)) -> if (s1=s2) then mgu Function(s1,tl1) Function(s2,tl2) else Substitution([])

let rec unifyClause atomic_formula clause = match (atomic_formula,clause) with
| (af,Fact(head) -> expr
| (AtomicFormula(s,lis),Rule(head,body) -> expr

let rec unifyAtomic atomic_formula program = match (atomic_formula,program) with

let rec unify goal program = match (goal,program) with
| (x::[],program) -> unifyAtomic x program
| (x::xs,program) -> (unify ([x]) program) (unify xs program)








let rec substitute subst1 t  = match (t,subst1) with
  | (V(Var(s)),Sub(x)) -> (try  List.assoc (Var(s)) x with | Not_found -> V(Var(s)))
  | (Node(x,y),Sub(z)) -> Node(x,map (substitute subst1) y)
;;

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
