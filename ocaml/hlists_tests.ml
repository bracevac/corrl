open Hlists

let l1 = HL.(cons 1 nil)
(*
  val l1 : (int * unit) HL.hlist = HL.S (1, HL.Z)
 *)

let l2 = HL.(cons true l1)

let l3 = HL.(cons "abc" l2)
(*
  val l3 : (string * (bool * (int * unit))) HL.hlist =
  HL.S ("abc", HL.S (true, HL.S (1, HL.Z)))
 *)

let n0 = Here
let n1 = Next n0
let n2 = Next n1

let _ = HLP.proj n0 l3
(* - : string = "abc" *)

let 1 = HLP.proj n2 l3

(*
let _ = HLP.proj n2 l1
Characters 16-18:
let _ = proj n2 d1
                ^^
Error: This expression has type (int * unit) drefs
     but an expression was expected of type (int * ('a * ('b * 'c))) drefs
     Type unit is not compatible with type 'a * ('b * 'c)
*)

let l3' = HLP.rplc n1 l3 false
let "abc" = HLP.proj n0 l3'
let false = HLP.proj n1 l3'
let 1 = HLP.proj n2 l3'

(*
let d3' = HLP.rplc n1 d3 (fun () -> 2)
Characters 32-33:
let d3' = rplc n1 d3 (fun () -> 2)
                                ^
Error: This expression has type int but an expression was expected of type
       bool
*)

let test_list = HLL.(cons [1;2;3] @@
                   cons ["A";"B"] @@ cons [0.3;0.2;0.1] @@ nil)
(*
val test_list : (int * (string * (float * unit))) HLL.hlist =
HLL.S ([1; 2; 3], HLL.S (["A"; "B"], HLL.S ([0.3; 0.2; 0.1], HLL.Z)))
*)


let _ =
  cart test_list (fun x -> [x])
(*
- : (int * (string * (float * unit))) list =
[(1, ("A", (0.3, ()))); (1, ("A", (0.2, ()))); (1, ("A", (0.1, ())));
(1, ("B", (0.3, ()))); (1, ("B", (0.2, ()))); (1, ("B", (0.1, ())));
(2, ("A", (0.3, ()))); (2, ("A", (0.2, ()))); (2, ("A", (0.1, ())));
(2, ("B", (0.3, ()))); (2, ("B", (0.2, ()))); (2, ("B", (0.1, ())));
(3, ("A", (0.3, ()))); (3, ("A", (0.2, ()))); (3, ("A", (0.1, ())));
(3, ("B", (0.3, ()))); (3, ("B", (0.2, ()))); (3, ("B", (0.1, ())))]
*)
