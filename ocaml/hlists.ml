(* Lightweight HLists *)
(*
#use "hlist_simple.ml";;
*)

module type hlist = sig
  type 'a el                            (* element type *)
  type _ hlist =
    | Z : unit hlist
    | S : 'a el * 'b hlist -> ('a * 'b) hlist

  val nil  : unit hlist
  val cons : 'a el -> 'b hlist -> ('a * 'b) hlist
end

module HList(E: sig type 'a t end) = struct
  type 'a el = 'a E.t
  type _ hlist =
    | Z : unit hlist
    | S : 'a el * 'b hlist -> ('a * 'b) hlist

  let nil = Z
  let cons h t = S (h,t)
end

(* Simple HList, just for elements of type 'a *)
module HL = HList(struct type 'a t = 'a end)

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

(* a pointer into an HList *)
type (_,_) ptr =
  | Here : ('a,'a*_) ptr
  | Next : ('a,'r) ptr -> ('a,_*'r) ptr

let n0 = Here
let n1 = Next n0
let n2 = Next n1

module HListP(H: hlist) = struct
  include H

  let rec proj : type a r. (a,r) ptr -> r hlist -> a el = fun n refs ->
    match (n,refs) with
    | (Here, S (f,_)) -> f
    | (Next n, S (_,refs)) -> proj n refs

  (* replacement *)
  let rec rplc : type a r. (a,r) ptr -> r hlist -> a el -> r hlist =
    fun n refs v ->
      match (n,refs) with
      | (Here, S (_,t))      -> S (v,t)
      | (Next n, S (h,refs)) -> S(h,rplc n refs v)
end

module HLP = HListP(HL)

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

(* Mapping, between element representations. Element type _index_
  should be preserved though (which means that the length is also
  preeserved)
*)
module HMAP(S:hlist)(T:hlist) = struct
  type ftor = {f: 'a. 'a S.el -> 'a T.el}
  let rec map : type a. ftor -> a S.hlist -> a T.hlist = fun {f} -> function
    | S.Z -> T.Z
    | S.S (h,t) -> T.S (f h, map {f} t)
end


(* HList of lists *)

module HLL = HList(struct type 'a t = 'a list end)

let test_list = HLL.(cons [1;2;3] @@
                     cons ["A";"B"] @@ cons [0.3;0.2;0.1] @@ nil)
(*
val test_list : (int * (string * (float * unit))) HLL.hlist =
  HLL.S ([1; 2; 3], HLL.S (["A"; "B"], HLL.S ([0.3; 0.2; 0.1], HLL.Z)))
*)

module HLLazy = HList(struct type 'a t = unit -> 'a list end)

(* Example of mapping *)
let force : 'a HLLazy.hlist -> 'a HLL.hlist = fun l ->
  let module M = HMAP(HLLazy)(HLL) in
  M.map {M.f = fun th -> th ()} l


(* Compute the cartesian product of HLL  *)


let rec cart : type w r. r HLL.hlist -> (r -> w list) -> w list = fun h f ->
  let open HLL in
  match h with
  | Z -> f ()
  | S (ls,h) ->
      List.concat @@ List.map (fun x -> cart h (fun xs -> f (x,xs))) ls


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
