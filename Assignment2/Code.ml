(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [
  ((3),S(S(S(Z))));
  ((0),(Z));
  ((5),S(S(S(S(S(Z))))));
] 

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let q1a_nat_of_int (n : int) : nat = 
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
let q1b_int_of_nat (n : nat) : int =
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
let q2a_neg (e : exp) : exp = Times(Const (-1.0), e)
  
  
(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = Plus(e1, q2a_neg(e2))

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp =
  let rec helper (power : int) (num : exp) (cur : exp) =
    match power with
    | 0 -> cur
    | _ -> helper (power - 1) (num) (Times(cur, num))
  in
  helper (q1b_int_of_nat (p)) (e1) (Const 1.)




(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  ((5., Plus(Const 7., Var)), 12.);
  ((3., (Plus(Plus(Times(Const 2., Var),Times(Const (-1.), Div(Var, Const 3.))),Const 10.))), 15.);
  ((-3., Plus(Const 0., Var)), -3.);
  ((2., Div(Const 0., Var)), 0.);
]

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float =
  match e with
  | Var -> a
  | Const c -> c 
  | Plus (e1,e2) -> ((eval a e1) +. (eval a e2)) 
  | Times (e1,e2) -> ((eval a e1) *. (eval a e2)) 
  | Div (e1,e2) -> ((eval a e1) /. (eval a e2))


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [
  ((Times(Const 3., Var)), (Plus (Times (Const 3., Const 1.), Times (Const 0., Var))));
  ((Div(Const 3., Var)), (Div(q2b_minus(Times((Const 0.), Var)) (Times((Const 1.), Const 3.)),Times(Var, Var))));
]

(* TODO: Implement {!diff}. *) 

let rec diff (e : exp) : exp =
  match e with
  | Var -> Const 1.
  | Const c -> Const 0.
  | Plus (e1,e2) -> Plus((diff e1),(diff e2))
  | Times (e1,e2) -> Plus((Times(e1, diff e2)),(Times((diff e1), e2)))
  | Div (e1,e2) -> Div(q2b_minus(Times((diff e1), e2)) (Times((diff e2), e1)),Times(e2,e2))
                      
