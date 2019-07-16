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

module HZIP(H1:hlist)(H2:hlist) = struct
  include HList(struct type 'a t = 'a H1.el * 'a H2.el end)
  let rec zip: type a. a H1.hlist -> a H2.hlist -> a hlist = fun h1 h2 ->
    match h1, h2 with
    | H1.Z, H2.Z -> Z
    | H1.(S (x1,h1)), H2.(S (x2,h2)) -> S ((x1,x2), zip h1 h2)
end

(* Simple HList, just for elements of type 'a *)
module HL = HList(struct type 'a t = 'a end)

module HPointers = struct
  (* Points-into relation *)
  type (_,_) _ptr =
    | Pz: ('a,'a * 'b) _ptr
    | Ps: ('a,'b) _ptr -> ('a, 'c * 'b) _ptr

  (* For encoding polymorphic judgment scheme of points-into relation *)
  type ('a,'b) ptr = unit -> ('a,'b) _ptr

  let pz () = Pz
  let ps p () = Ps (p ())

  module Vals = struct
    let n0 () = Pz
    let n1 () = Ps Pz
    let n2 () = Ps (Ps Pz)
    let n3 () = Ps (Ps (Ps Pz))
    let n4 () = Ps (Ps (Ps (Ps Pz)))
    let n5 () = Ps (Ps (Ps (Ps (Ps Pz))))
  end
  (* Submultiset relation  *)
  type (_,'a) _mptr =
    | Mz: (unit, 'a) _mptr
    | Ms: (('c, 'b) _ptr * ('a, 'b) _mptr) -> ('c * 'a, 'b) _mptr

  (* For encoding polymorphic judgment scheme of points-into relation *)
  type ('a,'b) mptr = unit -> ('a,'b) _mptr

  let mz () = Mz
  let ms p ps () = Ms (p (), ps ())

  (* Type-safe (multi)projection on hlists *)
  module Proj(H:hlist) = struct
    let rec proj: type a ctx. (a,ctx) _ptr -> ctx H.hlist -> a H.el =
      fun n hlist -> match n, hlist with
                     | Pz, H.S (hd, _) -> hd
                     | Ps n, H.S (_, tl) -> proj n tl

    let rec mproj: type xs ctx. (xs, ctx) _mptr -> ctx H.hlist -> xs H.hlist =
      fun mptr hlist -> match mptr with
                        | Mz -> H.nil
                        | Ms (i,ps) -> H.cons (proj i hlist) (mproj ps hlist)
  end
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
