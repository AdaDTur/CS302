type color = Red | Green | Yellow | Blue
type value = Numeric of int | Reverse | Skip | Plus2
type card = color * value 
            
            
type hand = Empty | Card of card * hand
let h1 = Card ( (Blue, Reverse), Card ( (Green, Skip), Empty) )
    
let rec nat2int (h : hand) : int = match h with
  | Empty -> 0
  | Card (_, h') -> 1 + nat2int h'
