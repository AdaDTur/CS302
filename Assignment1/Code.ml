(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [ 
  (((1, 2), (2, 4)), 3)
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance (x1, y1) (x2, y2) =
  if x1 < x2 && y1 < y2 then (x2 - x1) + (y2 - y1) else
  if x1 > x2 && y1 > y2 then (x1 - x2) + (y1 - y2) else
  if x1 > x2 && y1 < y2 then (x1 - x2) + (y2 - y1) else (x2 - x1) + (y1 - y2) ;;



(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 0);

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*) 
let factorial num =
  let rec go j acc =
    if j > 0 then go (j - 1) (acc * j) else acc
  in
  go num 1
    
let binomial n k =
  if (k != 0 && (n - k) != 0) then (factorial n) / ((factorial k) * (factorial (n - k))) else 0


(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
]

(* TODO: Implement a tail-recursive helper lucas_helper. *)
let rec lucas_helper params =
  raise NotImplemented


(* TODO: Implement lucas that calls the previous function. *)
let lucas n =
  raise NotImplemented
