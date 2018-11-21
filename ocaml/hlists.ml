(* Lightweight HLists *)
(* by Oleg Kiselyov with modifiations by Oliver Bracevac *)
(*
#use "hlist_simple.ml";;
 *)

(* #mod_use "monad.ml";; *)

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

(* a pointer into an HList *)
type (_,_) ptr =
  | Here : ('a,'a*_) ptr
  | Next : ('a,'r) ptr -> ('a,_*'r) ptr

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
      List.(concat @@ map (fun x -> cart h (fun xs -> f (x,xs))) ls)
