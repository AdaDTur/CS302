(*--------------------------------------------------------------*)
(* Q1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  let char_at_index s x = String.get s x in let x = String.length s in
  List.map (char_at_index s) (tabulate (fun i -> i) (String.length s))
  
(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  match l with
  | [] -> ""
  | _ -> List.fold_left (fun acc i -> acc ^ (Char.escaped i)) (Char.escaped (List.hd l)) (List.tl l)

(* ------------------------------------------------------------------------*)
(* Q2 : Bank Account *)
(* ------------------------------------------------------------------------*)

let open_account (pass : password) : bank_account =
  let balance = ref 0 in 
  let counter = ref 0 in 
  let refpass = ref pass in
  let update_pass (entered : password) (newpass : password) = 
    if entered = !refpass then 
      if !counter <= 2 then 
        refpass := newpass
      else raise account_locked
    else (raise wrong_pass ; counter := !counter + 1)
  in
  let deposit (entered : password) (depo : int) =
    if entered = !refpass then
      if !counter <= 2 then
        if depo < 0 then raise negative_amount else balance := !balance + depo
      else raise account_locked
    else (raise wrong_pass ; counter := !counter + 1)
  in
  let retrieve (entered : password) (ret : int) =
    if entered = !refpass then 
      if !counter <= 2 then
        if (!balance - ret) < 0 then raise not_enough_balance else
        if ret < 0 then raise negative_amount else balance := !balance - ret
      else raise account_locked
    else (raise wrong_pass ; counter := !counter + 1)
  in
  let show_balance (entered : password) : int =
    if entered = !refpass then 
      if !counter <= 2 then !balance
      else raise account_locked
    else (raise wrong_pass ; counter := !counter + 1)
  in 
  bank_account update_pass deposit retrieve show_balance
  
;;

