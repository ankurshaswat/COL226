#use "assi3.ml";;

let sig1 = Sig([(Sym("X"),Ar(0));(Sym("Y"),Ar(0));(Sym("f"),Ar(1));(Sym("g"),Ar(2));(Sym("h"),Ar(3));(Sym("*"),Ar(2))]);;
let sig2 = Sig([(Sym("X"),Ar(0));(Sym("Y"),Ar(0));(Sym("Z"),Ar(0));(Sym("f"),Ar(1));(Sym("g"),Ar(2));(Sym("f"),Ar(3));(Sym("*"),Ar(2))]);;
let sig3 = Sig([(Sym("f"),Ar(1))]);;
let sig4 = Sig([(Sym("X"),Ar(0));(Sym("Y"),Ar(0));(Sym("Z"),Ar(0))]);;

let term1 = (Node (Sym("f"),[V (Var("X"))]));;
let term2 = (Node (Sym "g",[V (Var("X"));Node(Sym "h",[Node(Sym "f",[V (Var("X"))]);V (Var("Y"))])]));;
let term3 = (Node (Sym "g",[V (Var("X"));Node(Sym "*",[V (Var("Y"));Node (Sym "*",[V (Var("X"));V (Var("Y"))])])]));;
let term4 = (Node (Sym "g",[V (Var("X"));Node(Sym "*",[V (Var("Y"));V (Var("X"))])]));;
let term5 = (Node (Sym "g",[V (Var("Z"));Node(Sym "*",[V (Var("X"));V (Var("Z"))])]));;
let term6 = (Node (Sym "g",[V (Var("Z"));Node(Sym "g",[V (Var("X"));V (Var("Z"))])]));;
let term7 = (V (Var("X")));;
let term8 = (Node (Sym "K",[]));;
let term9 = (Node (Sym "X",[]));;
let term10 = (Node (Sym "g",[V (Var("X"));Node(Sym "h",[Node(Sym "f",[V (Var("X"))]);V (Var("Y"));Node (Sym "X",[])])]));;
let term11 = (Node (Sym "g",[V (Var("X"));Node(Sym "h",[Node(Sym "f",[V (Var("X"))]);V (Var("Y"));Node (Sym "f",[V (Var("X"))])])]));;
let term12 = (Node (Sym "g",[V (Var("Z"));Node(Sym "*",[V (Var("Z"));Node (Sym "*",[V (Var("X"));V (Var("Y"))])])]));;
let term13 = (Node (Sym "$",[V (Var("P"));V (Var("Q"))]));;
let term14 = (Node (Sym "$",[Node (Sym "2",[]); Node (Sym "4",[])]));;
let term15 = (Node (Sym "$",[Node (Sym "2",[]); Node (Sym "3",[])]));;

Printf.printf "(1)check_sig sig1 : %B\n" (check_sig sig1);;
Printf.printf "(2)check_sig sig2 : %B\n" (check_sig sig2);;
Printf.printf "(3)check_sig sig3 : %B\n" (check_sig sig3);;
Printf.printf "(4)check_sig sig4 : %B\n\n" (check_sig sig4);;

Printf.printf "(5)wfterm term1 sig1 : %B\n" (wfterm sig1 term1);;
Printf.printf "(6)wfterm term2 sig1 : %B\n" (wfterm sig1 term2);;
Printf.printf "(7)wfterm term7 sig4 : %B\n" (wfterm sig4 term7);;
(* Printf.printf "(8)wfterm term8 sig4 : %B\n" (wfterm sig4 term8);; *)
Printf.printf "(9)wfterm term9 sig4 : %B\n\n" (wfterm sig4 term9);;

Printf.printf "(10)ht term9 : %d\n" (ht term9);;
Printf.printf "(11)ht term7 : %d\n" (ht term7);;
Printf.printf "(12)ht term4 : %d\n" (ht term4);;
Printf.printf "(13)ht term10 : %d\n" (ht term10);;
Printf.printf "(14)ht term11 : %d\n\n" (ht term11);;

Printf.printf "(15)size term9 : %d\n" (size term9);;
Printf.printf "(16)size term7 : %d\n" (size term7);;
Printf.printf "(17)size term4 : %d\n" (size term4);;
Printf.printf "(18)size term10 : %d\n" (size term10);;
Printf.printf "(19)size term11 : %d\n\n" (size term11);;

Printf.printf "(20)vars term9 : ";; (vars term9);; Printf.printf("\n");;
Printf.printf "(21)vars term7 : ";; (vars term7);; Printf.printf("\n");;
Printf.printf "(22)vars term4 : ";; (vars term4);; Printf.printf("\n");;
Printf.printf "(23)vars term10 : ";; (vars term10);; Printf.printf("\n");;
Printf.printf "(24)vars term11 : ";; (vars term11);; Printf.printf("\n\n");;


Printf.printf "(31)mgu term14 term13 : ";; (mgu term14 term13);; Printf.printf("\n");;
Printf.printf "(33)mgu term3  term12 : ";; ((mgu term3 term12));; Printf.printf("\n");;
Printf.printf "(34)mgu term12 term3  : ";; ((mgu term12 term3));; Printf.printf("\n\n");;

Printf.printf "(33.1)subst term12 (mgu term3 term12)  : ";; (subst (mgu term3 term12) term12);; Printf.printf("\n");;
Printf.printf "(33.2)subst term3  (mgu term3 term12)  : ";; (subst (mgu term3 term12) term3);; Printf.printf("\n\n");;

Printf.printf "(34.1)subst term12 (mgu term12 term3)  : ";; (subst (mgu term12 term3)  term12 );; Printf.printf("\n");;
Printf.printf "(34.2)subst term3  (mgu term12 term3)  : ";; (subst (mgu term12 term3) term3 );; Printf.printf("\n\n");;
