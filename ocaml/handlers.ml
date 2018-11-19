(* binary handler composition *)
let (|+|) h1 h2 action = h1 (fun () -> h2 action)
(* compose given list of handlers to a single handler.  *)
let comp hs = List.fold_right (|+|) hs (fun action -> action ())
(* single handler application *)
let with_h h action = h action
(* multiple handler application  *)
let with_hs hs action = with_h (comp hs) action
(* apply handler-generating function to a list of things and compose *)
let gen list f = comp (List.mapi f list)
