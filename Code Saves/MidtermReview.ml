(* PART 1: backtracking *)

(* Section 1.1: making change *)

exception Change

let rec change (coins : int list) (amt : int) : int list =
  match coins with
  | _ when amt = 0 -> []
  | [] -> raise Change
  | c :: cs when c > amt ->
      change cs amt
  | c :: cs ->
      try
        c :: change cs (amt - c)
      with
        Change -> change cs amt
      
                    (*Option based*)
let rec change_opt (coins : int list) (amt : int) : int list option =
  match coins with
  | _ when amt = 0 -> Some []
  | [] -> None
  | c :: cs when c > amt -> change_opt cs amt
  | c :: cs ->
      match change_opt (c :: cs) (amt - c) with
      | Some soln -> c :: soln
      | None -> change_opt cs amt
                  
                  (* cps solution *)
let rec change_cps (coins : int list) (amt : int) (return : int list -> 'r) (backtrack : unit 'r) : 'r =
  match coins with
  | _ when amt = 0 -> return []
  | [] -> backtrack ()
  | c :: cs when c > amt -> change_cps cs amt return backtrack
  | c :: cs ->
      change_cps (c :: cs) (amt - c)
        (fun soln -> return (c :: soln))
        (fun () -> change_cps cs amt return backtrack)
(* Section 1.2: I can't get no satisfaction *)

type formula =
  | Conj of formula * formula
  | Disj of formula * formula
  | Neg of formula
  | Var of string

type truth_assignment = (string * bool) list

(* Evaluates a formula with a given assignment for the values of the variables. *)
let rec eval (env : truth_assignment) (phi : formula) : bool = failwith "as before"

(* All the variable names that appear in a formula. *)
let rec collect_variables (phi : formula) : string list = failwith "as before"

exception Unsatisfiable

let find_satisfying_assignment_opt (phi : formula) : truth_assignment option =
  let vars = collect_variables phi in
  let go (vars : string list) (ta : truth_assignment) : truth_assignment = 
    match vars with
    | [] -> if eval ta phi then ta else raise Unsatisfiable
    | x :: xs -> 
        try go xs ((x, true) :: ta) with
        | Unsatisfiable -> go xs ((x, false) :: ta)
  in
  go vars []

let find_satisfying_assignment_cps (phi : formula)
    (return : truth_assignment -> 'r) (backtrack : unit -> 'r) : 'r =
  let vars = collect_variables phi in
  let go (vars : string list)  (ta : truth_assignment) (backtrack : unit 'r) 'r =
    match vars with
    | [] -> if eval ta phi then return ta else backtrack ()
    | x :: xs ->
        go xs ((x, true) :: ta) return
          (fun () -> go xs ((x, false) :: ta) return backtrack)
  in
  go vars [] return backtrack

(* PART 2: lazy programming with streams *)

type 'a susp = Susp of (unit -> 'a)
type 'a str = {
  hd : 'a;
  tl : 'a str susp;
}
let force (Susp f) = f ()

let rec take n s = if n = 0 then [] else s.hd :: take (n-1) (force s.tl)

let rec unfold (f : 'state -> 'elem * 'state) (s : 'state) : 'elem str =
  let (x, s') = f s in {
    hd = x;
    tl = Susp (fun () -> unfold f s')
  }

(* Section 2.1: Implement map for streams using `unfold`. *)
let rec map (f : 'a -> 'b) (s : 'a str) : 'b str = failwith "exercise"

(* Section 2.2: Implement a specific stream. *)

(** The Wallis product, published in 1656, is an infinite product
    whose limit is pi/2. It is among the oldest known ways to
    calculate pi to arbitrary precision.

    It is defined as:
      (2/1 * 2/3) * (4/3 * 4/5) * (6/5 * 6/7) * (8/7 * 8/9) * ...

    Notice that the nth factor in the product is given by the formula:
      a_n = 2n/(2n-1) * 2n/(2n+1)
          = 4n^2/(4n^2 - 1)
    (Remark: this sequence begins at n=1 !)

    Denote by W_n the Wallis product truncated at factor n. So:
        W_1 = a_1 = 2/1 * 2/3
        W_2 = a_2 * W_1
        W_3 = a_3 * W_2 = a_3 * a_2 * a_1
    and so on.  *)

let rec seq (f : int -> 'a) : 'a str = unfold (fun n -> (f n, n + 1)) 0

(* Define a recursive function `f` such that `f n` computes W_n as above.
   Then, use `seq f` to compute the stream of successive approximations of the Wallis product. *)

let wallis_1 =
  let rec f n = failwith "exercise" in
  seq f

(** This previous method is quite inefficient, since it recalculates
... (35 lines left)
