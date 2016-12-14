open OUnit

let all_pairs =
  let every_state = Vdi_automaton.([
      Detached;
      Attached RO; Attached RW;
      Activated RO; Activated RW
    ]) in
  let every_op = Vdi_automaton.([
      Nothing;
      Attach RO; Attach RW;
      Activate;
      Detach; Deactivate;
    ]) in
  let all_pairs xs ys =
    List.map (fun x -> List.map (fun y -> (x, y)) ys) xs |> List.flatten
  in
  (all_pairs every_state every_op)

(* For any state [s] and operation [o] where [s' = s + o],
   [if s <> s' then s - s' = op] *)
let test () =
  let open Vdi_automaton in
  List.iter
    (fun (s, op) ->
       try
         let s' = s + op in
         let op' = List.map fst (s - s') in
         if s <> s' && [ op ] <> op'
         then failwith (Printf.sprintf "s = %s; op = %s; s + op = %s; s - (s + op) = %s"
                          (string_of_state s) (string_of_op op)
                          (string_of_state s')
                          (String.concat ", " (List.map string_of_op op')))
       with Bad_transition(_, _) -> ()
    ) all_pairs

let tests =
  "vdi_automaton" >:::
  [
    "Test VDI automaton" >:: test;
  ]
