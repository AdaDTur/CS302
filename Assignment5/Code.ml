(** Question 1 *)

(* TODO: Add test cases. *)
let collect_variables_tests : (formula * Variable_set.t) list = [
  ((Conjunction (Variable "A", Variable "B")), (Variable_set.of_list ["A"; "B"]));
  ((Disjunction (Conjunction (Variable "A", Variable "B"), Negation (Variable "C"))), (Variable_set.of_list ["A"; "B";"C"]))
]

(* TODO: Implement the function. *)
let rec collect_variables (formula : formula) : Variable_set.t =
  match formula with
  | Variable x -> Variable_set.add x Variable_set.empty
  | Conjunction (form1, form2) ->
      let form11 = collect_variables form1 in
      let form21 = collect_variables form2 in
      Variable_set.union form11 form21
  | Disjunction (form1, form2) -> 
      let form11 = collect_variables form1 in
      let form21 = collect_variables form2 in
      Variable_set.union form11 form21
  | Negation form1 -> collect_variables form1
                        

(** Question 2 *)

(* TODO: Add test cases. *)
let eval_success_tests : ((truth_assignment * formula) * bool) list = [
  (((Variable_map.singleton "A" true), (Variable "A")), (true));
  (((Variable_map.singleton "A" false |> Variable_map.add "B" false),(Conjunction (Variable "A", Variable "B"))), (false)); 
  (((Variable_map.singleton "A" true
     |> Variable_map.add "B" false
     |> Variable_map.add "C" false, ((Disjunction (Conjunction (Variable "A", Variable "B"), Negation (Variable "C"))))), (true)));
]

(* TODO: Add test cases. *)
let eval_failure_tests : ((truth_assignment * formula) * exn) list = [
  (((Variable_map.empty), (Variable "A")), (Unassigned_variable "A"));
  (((Variable_map.singleton "A" true), (Variable "B")), (Unassigned_variable "A"));
  (((Variable_map.singleton "A" true
     |> Variable_map.add "B" false
     |> Variable_map.add "C" false, ((Disjunction (Conjunction (Variable "A", Variable "D"), Negation (Variable "C"))))), (Unassigned_variable "D")));
  (((Variable_map.singleton "D" true
     |> Variable_map.add "B" false
     |> Variable_map.add "C" false, ((Conjunction (Disjunction (Variable "A", (Variable "A")), (Variable "A"))))), (Unassigned_variable "A")));

]

(* TODO: Implement the function. *)
let rec eval (state : truth_assignment) (formula : formula) : bool =
  match formula with
  | Variable x -> 
      (match Variable_map.find_opt x state with
       | None -> raise (Unassigned_variable x)
       | Some x -> x)
  | Conjunction (form1, form2) -> let func1 = eval state form1 in let func2 = (eval state form2) in func1 && func2
  | Disjunction (form1, form2) -> (eval state form1) || (eval state form2)
  | Negation x -> not (eval state x)

(** Question 3 *)

(* TODO: Add test cases. *)
let find_satisfying_assignment_tests : (formula * truth_assignment option) list = [
  ((Variable "A"), (Some (Variable_map.singleton "A" true)));
  ((Conjunction (Variable "A", Negation (Variable "A"))), (None));
  ((Disjunction (Conjunction (Variable "A", Negation (Variable "B")), Variable "C")), (Some (Variable_map.singleton "A" true |> Variable_map.add "B" false |> Variable_map.add "C" false)));
]

(* TODO: Implement the function. *)
let find_satisfying_assignment (formula : formula) : truth_assignment =
  let rec helper set state formula =
    match Variable_set.elements set with
    | [] -> if eval state formula then state else raise Unsatisfiable_formula
    | x :: xs -> 
        try helper (Variable_set.of_list xs) (Variable_map.add x true state) formula with
        | Unsatisfiable_formula -> helper (Variable_set.of_list xs) (Variable_map.add x false state) formula
  in
  helper (collect_variables formula) Variable_map.empty formula
    
    





  
