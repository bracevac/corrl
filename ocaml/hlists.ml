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

  let rec length: type a. a hlist -> int =
    function
    | Z -> 0
    | S (_,hs) -> 1 + (length hs)
end

(* Mapping, between element representations. Element type _index_
  should be preserved though (which means that the length is also
  preserved)
*)
module HMAP(S:hlist)(T:hlist) = struct
  type ftor = {f: 'a. 'a S.el -> 'a T.el}
  let rec map : type a. ftor -> a S.hlist -> a T.hlist = fun {f} -> function
    | S.Z -> T.Z
    | S.S (h,t) -> T.S (f h, map {f} t)
end

module HFOLD(H:hlist) = struct
  type 'a fold = {zero: 'a; succ: 'b. 'b H.el -> (unit -> 'a) -> 'a }
  let rec fold: type a b. a fold -> b H.hlist -> a = fun {zero;succ} ->
    function
    | H.Z -> zero
    | H.S (x,t) -> succ x (fun () -> fold {zero;succ} t)
end

module HFOREACH(H:hlist) = struct
  type foreach = {f: 'a. 'a H.el -> unit}
  let foreach: type a. foreach -> a H.hlist -> unit = fun {f} hs ->
    let module Units = HList(struct type 'a t = unit end) in
    let module M = HMAP(H)(Units) in
    ignore @@ M.map {M.f = f} hs
end

(* Simple HList, just for elements of type 'a *)
module HL = HList(struct type 'a t = 'a end)

(* a pointer into an HList *)
type (_,_) ptr =
  | Here : ('a,'a*_) ptr
  | Next : ('a,'r) ptr -> ('a,_*'r) ptr

(* Multisets of pointers *)
module Ptrs = struct
  (* Second type parameter enforces that all pointers point into the same context. *)
  type (_,'a) hlist =
    | Z: (unit,'a) hlist
    | S: (('i,'a) ptr * ('j,'a) hlist) -> ('i * 'j, 'a) hlist

  let nil () = Z
  let cons p ps = S (p (), ps) (* Note the difference to other hlists, which is due to the value restriction *)

  let n0 () = Here
  let n1 () = Next Here
  let n2 () = Next (Next Here)
  let n3 () = Next (Next (Next Here))
  let n4 () = Next (Next (Next (Next Here)))
  let n5 () = Next (Next (Next (Next (Next Here))))
end

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

module HListPs(H: hlist) = struct
  include HListP(H)
  let proj_ptrs: type xs ctx. ctx hlist -> (xs,ctx) Ptrs.hlist -> xs hlist = fun hs ->
    let rec aux: type xs. (xs,ctx) Ptrs.hlist -> xs hlist =
      function
      | Ptrs.Z -> nil
      | Ptrs.(S (i,ps)) -> cons (proj i hs) (aux ps)
    in
    aux
end

module HLP = HListP(HL)


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
