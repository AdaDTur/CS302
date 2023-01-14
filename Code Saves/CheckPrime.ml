let is_prime n =
  if n < 2 then failwith "no can do" else
    let rec go (i : int) : bool = 
  (* idea: i starts at 2 and we're counting up, so we're initially calling
     `is_prime 2 n` for some n.
     Along the way, we check if `n mod i = 0` i.e. whether i evenly
     divides n. If it does, then n isn't prime; else make a recursive call
     increasing `i`. *)
      if i = n then true else 
    (* if we counted all the way up to `n`, then we ensured that 2...(n-1) don't
    divide n, so n is prime. *)
    (* up to you to code the rest! *)
      if n mod i = 0 then false else
        go (n+1)
    in
    go 2


