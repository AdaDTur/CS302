type rps = Rock | Paper | Scissors
type outcome = Win | Draw | Loss
  
(* Option 1 *)
let play (p1 : rps) (p2 : rps) : outcome =
  if p1 = p2 then Draw else
    match (p1, p2) with 
    | (Rock, Paper) -> Loss | (Rock, Scissors) -> Win 
    | (Scissors, Rock) -> Loss | (Scissors, Paper) -> Win 
    | (Paper, Scissors) -> Loss | (Paper, Rock) -> Win 
  
(* Option 2 *)
let play (p1 : rps) (p2 : rps) : outcome =
  match (p1, p2) with 
  | (Rock, Paper) -> Loss | (Rock, Scissors) -> Win 
  | (Scissors, Rock) -> Loss | (Scissors, Paper) -> Win 
  | (Paper, Scissors) -> Loss | (Paper, Rock) -> Win
  | _ when p1 = p2 -> true
