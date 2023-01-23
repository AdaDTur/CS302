let rec filter (p : 'a ->bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs ->
      if p x then
        let ys = filter p xs in
        x :: ys
      else
        filter p xs
          
let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | x :: xs -> 
      (* x : 'a, xs : 'a list *)
      (* f x : 'b *)
      f x :: map f xs
        
let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (e : 'b) =
  match l with
  | [] -> e
  | x :: xs -> f x (fold_right f xs e)
