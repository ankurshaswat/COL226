let teaches1 =Fact(Function("teaches", [Function("dr_fred", []); Function("history", [])]));;
let teaches2 =Fact(Function("teaches", [Function("dr_fred", []); Function("english", [])]));;
let teaches3 =Fact(Function("teaches", [Function("dr_fred", []); Function("drama", [])]));;
let teaches4 =Fact(Function("teaches", [Function("dr_fiona", []); Function("physics", [])]));;
let studies1 =Fact(Function("studies", [Function("alice", []); Function("english", [])]));;
let studies2 =Fact(Function("studies", [Function("angus", []); Function("english", [])]));;
let studies3 =Fact(Function("studies", [Function("amelia", []); Function("drama", [])]));;
let studies4 =Fact(Function("studies", [Function("alex", []); Function("physics", [])]));;

let prog = [teaches1;teaches2;teaches3;teaches4;studies1;studies2;studies3;studies4];;

let cut = Function("CUT",[]);;
let goal1 = Function("teaches", [Function("dr_fred", []); V(Var("Course"))]);;
let goal2 = Function("studies", [V(Var("Student")); V(Var("Course"))]);;

run_prolog [goal1] prog;;
run_prolog [goal2] prog;;
run_prolog [goal1;goal2] prog;
unify 0 [goal1;cut;goal2] prog prog;;
unify 0 [cut;goal1;goal2] prog prog;;
unify 0 [goal1;goal2;cut] prog prog;;
run_prolog [goal1;goal2;cut] prog;;
run_prolog [goal1;cut;goal2] prog;;
