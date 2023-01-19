let zero = Z
let one = S Z
let two = S (S Z)
let three = S two
    
let rec nat2int (n : nat) : int = match n with
  | Z -> 0
  | S n -> 1 + nat2int n

(* Finish later *)
let rec nat2int' (n : nat) (sum : int) : int = ...
