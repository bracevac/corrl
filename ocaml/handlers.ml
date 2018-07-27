(* apply the given list hs of effect handler thunks to action thunk  *)
let with_h hs action =
  let comp h thnk = (fun () -> (h thnk)) in
  List.fold_right comp hs action

(* compose given list of handlers to a single handler. TODO: use metaocaml magic to elim. overhead  *)
let hcomp hs =
  let comp h hres = (fun action -> h (fun () -> hres action)) in
  List.fold_right comp hs (fun action -> action ())

(* apply handler-generating function to a list of things and compose *)
let gen_handlers list f = hcomp (List.mapi f list)

(* dito for array of handlers, strangely enough, mapping array to list and then with_h results in segfault*)
let with_ha hs action =
  let comp h thnk = (fun () -> (h thnk)) in
  Array.fold_right comp hs action
