type color = Red | Green | Yellow | Blue
type value = Numeric of int | Reverse | Skip | Plus2
type card = color * value
            
let can_follow (c1 : card) (c2 : card) = 
  let (col1, val1) = c1 in
  let (col2, val2) = c2 in
  col1 = col2 || val1 = val2
                 
let can_follow c1 c2 = 
  match c1 with
  | (col1, val1) ->
      match c2 with
      | (col2, val2) -> failwith "same as before"
                          
let can_follow c1 c2 =
  match (c1, c2) with
  | ((col1, val1), (col2, val2)) -> col1 = col2 || val1 = val2
                                                   
let is_fav_color c = match c with
  | Red -> true
  | _ -> false
    
let is_fav_card v = match v with
  | Numeric _ -> true
  | _ -> false
