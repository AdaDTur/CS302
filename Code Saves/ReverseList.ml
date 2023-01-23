(* Non-tail recursive *)
let rec reverse (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs -> rev xs @ [x]
  
(* Tail recursive *)
let rec rev (l : 'a list) (acc : 'a list) : 'a list =
  match l with
  | [] -> acc
  | x :: xs -> rev xs (x :: acc)
