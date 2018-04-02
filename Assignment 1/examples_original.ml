#use "submission.ml";;

let alphabet=["1"; "2"; "a"; "b"; "c"; "A"];;

(* let empty = create "";; *)
let a = create "a";;
let abc = create "abc";;
let one2 = create "12";;
let one = create "1";;
let oneA = create "1A";;

(* lgh empty;; *)
lgh a;;
lgh abc;;
lgh one2;;

(* nonempty empty;; *)
nonempty a;;
nonempty one2;;

(* print (concat empty empty);; *)
(* print (concat empty a);; *)
(* print (concat one empty);; *)
print (concat oneA abc);;

(* print (reverse empty);; *)
print (reverse abc);;
print (reverse one2);;

let abc = create "abc";;
let one2 = create "12";;

(* first empty;; *)
first a;;
first abc;;

(* last empty;; *)
last a;;
last abc;;

let editable = create "abac12a2aAac211";;

forward editable;;
back editable;;
moveTo 10 editable;;
replace 'b' editable;;

print editable;;
