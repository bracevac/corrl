(* apply the given list hs of effect handler thunks to action thunk  *)
let with_h hs action =
  let comp h thnk = (fun () -> (h thnk)) in
  List.fold_right comp hs action

(* dito for array of handlers, strangely enough, mapping array to list and then with_h results in segfault*)
let with_ha hs action =
  let comp h thnk = (fun () -> (h thnk)) in
  Array.fold_right comp hs action
