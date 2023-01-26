(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [
  ((3),S(S(S(Z))));
  ((0),(Z));
  ((5),S(S(S(S(S(Z))))));
] 

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_nat_of_int (n : int) : nat = 
  let rec helper (cur : nat) (num : int) =
    match num with
    | 0 -> cur
    | _ -> helper (S(cur)) (num - 1) 
  in
  helper Z n
    

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [
  ((Z),(0));
  (S(S(S(Z))),(3));
  (S(S(S(S(S(Z))))),(5));
]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_int_of_nat (n : nat) : int =
  let rec helper (cur : int) (num : nat) =
    match num with
    | Z -> cur
    | S num -> helper (cur + 1) (num) 
  in
  helper 0 n

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((Z,S(Z)),(S(Z)));
  ((S(Z),S(Z)),(S(S(Z))));
]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat =
  match n with
  | Z -> m
  | S n -> q1c_add n (S(m))
  


(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = raise Not_implemented

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = raise Not_implemented

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp = raise Not_implemented


(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = []

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = raise Not_implemented


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = []

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = raise Not_implemented
