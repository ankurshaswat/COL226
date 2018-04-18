type term = Var | Const | Function of string * (term list);;

type atomic_formula = AtomicFormula of string * (term list);;

type head = atomic_formula;;
type body = atomic_formula list;;

type clause = Fact of head
            | Rule of head * body ;;

type program = clause list;;

type goal = atomic_formula list;;
