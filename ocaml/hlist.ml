
open Higher

(* Simple heterogeneous list GADT *)
(* TODO overload standard list syntax *)
type _ hlist =
  | HNil : unit hlist
  | HCons : ('a * 'b hlist) -> ('a * 'b) hlist

let rec hlength: type a. a hlist -> int = function
  | HNil -> 0
  | HCons (_, hs) -> 1 + (hlength hs)

type 'f inj = { inj: 'a. 'a -> ('a, 'f) app }
(*type ('f, 't) prj = { prj: 'a. ('a, 'f) app -> 't }*)


(* Generate an hlist containing a polymorphic 'f inj function n times *)
module Repeat = struct
  let z f = HNil
  let s n ({inj} as fn) =
    let fs = n fn in
    HCons (inj, fs)

  (* tests *)
  let once = s z
  let twice = s (s z)
  let thrice = s (s (s z))
end

(* Polyvariadic hlist map: (a1 -> b1 * ... * an -> bn) hlist -> (a1 * ... * an) hlist -> (b1 * ... * bn) hlist  *)
module HListMap = struct
  let z: unit hlist -> unit hlist -> unit hlist = (fun HNil HNil -> HNil)

  (* let s (type a) (type b) (type c) (type d) (type e)
   *       (n: a hlist -> b hlist -> c hlist)
   *       (fs: ((d -> e) * a) hlist)
   *       (hs: (d * b) hlist): (e * c) hlist = *)
  let s n fs hs =
    match fs with
    | HCons (f, fs') ->
       match hs with
         HCons (x, hs') ->
         let rs = n fs' hs' in
         HCons (f x, rs)

  let map n fs hs = n fs hs

  (* tests *)
  let once = s z
  let twice = s (s z)
  let thrice = s (s (s z))
end

module HListFoldR = struct
  let z: type a. unit hlist -> unit hlist -> a -> a = (fun _ _ x -> x)
  (* let s (type a) (type b) (type c) (type d) (type e) (type f)
   *       (n: a hlist -> b hlist -> c -> d)
   *       (fs: ((e -> d -> f) * a) hlist)
   *       (hs: (e * b) hlist)
   *       (c: c): f = *)
  let s n fs hs c =
    match fs with
    | HCons (f, fs') ->
       match hs with
         HCons (x, hs') ->
         let rs = n fs' hs' c in
         f x rs

  let foldr n fs hs = n fs hs

  (* tests *)
  (* let once = s z
   * let twice = s (s z)
   * let thrice = s (s (s z)) *)
end

module HListFoldL = struct
  let z: type a. unit hlist -> unit hlist -> a -> a = (fun _ _ x -> x)
  let s n fs hs c =
    match fs with
    | HCons (f, fs') ->
       match hs with
         HCons (x, hs') ->
         n fs' hs' (f c x)

  let foldl n fs hs = n fs hs

  (* tests *)
  (* let once = s z
   * let twice = s (s z)
   * let thrice = s (s (s z)) *)
end

module HListProjection = struct
  let z =   (fun (HCons (hd,_)) -> hd)
  let s n = (fun (HCons (_,hs)) -> n hs)

  (* let once = s z
   * let twice = s (s z)
   * let thrice = s (s (s z)) *)
end

module HListTake = struct
  let z =   (fun _ -> HNil)
  let s n = (fun (HCons (hd,hs)) -> HCons (hd, n hs))
  (* tests *)
  (* let once = s z
   * let twice = s (s z)
   * let thrice = s (s (s z)) *)
end

module HListDrop = struct
  let z =   (fun hs -> hs)
  let s n = (fun (HCons (hd,hs)) -> n hs)
  (* tests *)
  (* let once = s z
   * let twice = s (s z)
   * let thrice = s (s (s z)) *)
end

(* Polyvariadic, position-dependent definitions *)

(* First, zippers, focus is on the head of the right component *)
module HListZipper = struct
  let z = (fun (HCons (x,hs)) -> (HNil, (HCons (x,hs))))
  let s (* (type a) (type b) (type c) (type d) (type e)
         * (n: (a * b) hlist -> (c hlist * (d * e) hlist))
         * (hs: (a * b) hlist) = *)
    n hs =
    let (left, HCons(x,right)) = n hs in
    (HCons(x,left), right)

  let zipper n hs = n hs

  (* tests *)
  (* let once = s z
   * let twice = s (s z)
   * let thrice = s (s (s z)) *)
end

(* Like a zipper, but the left part in original order *)
module HListSplitter = struct
  let z = (HListProjection.z, HListTake.z, HListDrop.z)
  let s (p,t,d) = (HListProjection.s p, HListTake.s t, HListDrop.s d)

  let split (np, nt, nd) hs = (nt hs, np hs, HListDrop.(s nd) hs)

  let hlist  = (HCons (1, HCons (2, HCons (3, HCons (4, HNil)))))
  let once = split (s z) hlist
  let twice = split (s (s z)) hlist
  let thrice  = split (s (s (s z))) hlist
end

module Range = struct
  (*This won't work, because types get non-uniform *)
  (* let z z' s' = HCons (z', HNil)
   * let s n z' s' =
   *   let HCons (n', tl) = n z' s' in
   *   HCons (s' n', HCons (n', tl)) *)

  let z z' s' = (HCons (z', HNil), s' z', s')
  let s n =
    let (hs, n', s') = n in
    (HCons (n', hs), s' n', s')
end


(* (\* The hlist of all splits of an hlist  *\)
 * module HListSplitters = struct
 *   let z ((HCons _) as hs) = ((fun tl -> HCons ((HListSplitter.(split z hs)), tl)), HListSplitter.z)
 *   let s n ((HCons _) as hs) =
 *     let (f_tl, pred) = n hs in
 *     let succ = HListSplitter.s pred in
 *     let hs' = HListSplitter.(split succ) hs in
 *     ((fun tl -> f_tl (HCons (hs', tl))), succ)
 *
 *   let splitters n hs = (fst (n hs)) HNil
 *
 *   (\* tests *\)
 *   let hlist  = (HCons (1, HCons (2, HCons (3, HCons (4, HNil)))))
 *   let once   = splitters z hlist
 *   let twice  = splitters (s z) hlist
 *   let thrice = splitters (s (s z)) hlist
 *   let fourth = splitters (s (s (s z))) hlist
 * end *)

(* Second, enumerate the first n zippers into an hlist *)
module HListZipperList = struct
  (* TODO: it seems there is some variant of finally tagless lurking in these definitions,
perhaps we could use that style for composition and overcome the "no uniform numeral representation limitation". *)
  let z hs = ((fun tl -> HCons (HListZipper.z hs, tl)), HListZipper.z)
  let s n hs =
    let (f_tl, pred) = n hs in
    let succ = HListZipper.s pred in
    ((fun tl ->  f_tl (HCons (succ hs, tl))), succ)

  (* TODO: could the length be inferred somehow by the argument list? *)
  let zippers n hs = (fst (n hs)) HNil

  (* tests *)
  let hlist  = (HCons (1, HCons (2, HCons (3, HCons (4, HNil)))))
  let once   = zippers z hlist
  let twice  = zippers (s z) hlist
  let thrice = zippers (s (s z)) hlist
  let fourth = zippers (s (s (s z))) hlist
end

(* Finally, position-dependent mapping over the list of zippers *)
module HListZipperMap = struct
  module M  = HListMap
  module Zl = HListZipperList

  let z   = (M.(z), Zl.z)
  let s n = (M.(s (fst n)), Zl.(s (snd n)))

  (* weird: we cannot put the increment of s_m into the definition of the z case,
type inference will not work properly. *)
  let map_zippers (m_n, zl_n) fs hs =
    M.map M.(s m_n) fs (Zl.(zippers zl_n hs))

  (* tests *)
  (* let once = map_zippers (s z)
   * let twice = map_zippers (s (s z))
   * let thrice = map_zippers (s (s (s z))) *)
end

(* emulate the type constructors in our framework *)
type 'a r = 'a list
type 'a evt = 'a

(* to construct type witnesses and map types to other types *)
type 'a typ = Typ
let witness  : 'a -> 'a typ          = (fun _ -> Typ)
let evt_typ  : 'a typ -> 'a evt typ  = (fun _ -> Typ)
let tparam_of: 'a r typ -> 'a typ    = (fun _ -> Typ)
let list_typ : 'a typ -> 'a list typ = (fun _ -> Typ)

module type ElementWise = sig
  type 'a wrap
  (* (t,u) proof iff t and u are hlist phantom types (i.e. nested tuple types)
     with (1) same length and (2) u_i = t_i wrao, i.e., u is type constructor (_ wrap) applied to t elementwise.   *)
  type (_, _) proof =
    | Base: (unit, unit) proof
    | Step: ('a typ * 'a wrap typ * ('b, 'c) proof) -> (('a * 'b), ('a wrap * 'c)) proof
end

module ElementWiseWrap(T: sig type 'a wrap end): (ElementWise with type 'a wrap = 'a T.wrap) =
  struct
    type 'a wrap = 'a T.wrap

    type (_, _) proof =
      | Base: (unit, unit) proof
      | Step: ('a typ * 'a wrap typ * ('b, 'c) proof) -> (('a * 'b), ('a wrap * 'c)) proof
  end

(* Proves that all elements of an hlist are of the shape (a_i, 'f) app for a given type constructor 'f. *)
module HElemApp1 = struct
  (* 1st arg: the hlist type, 2nd arg: the type constructor *)
  type (_,_) proof =
    | Base: (unit, 'f) proof
    | Step: ('a, 'f) proof -> (('b, 'f) app * 'a, 'f) proof
end


(* Elementwise project hlists of the form  <(ai, f) app> to the underlying type. *)
(* TODO this is further generalizable *)
module HElemProj1(N: Newtype1) = struct
  (* ('a, b') proof iff 'a is hlist type of the form <(a_i, N.t) app> and 'b
     is the corresponding projected hlist type <a_i N.s>. *)
  type (_,_) proof =
    | Base: (unit, unit) proof
    | Step: ('a, 'b) proof -> (('c, N.t) app * 'a, 'c N.s * 'b) proof

  let rec project: type a b. (a, b) proof -> a hlist -> b hlist =
    (fun proof hlist ->
      match (proof, hlist) with
      | (Base, HNil) -> HNil
      | (Step proof', HCons (x, xs)) ->
         HCons (N.prj x, project proof' xs))
end

(* Numerals version without proof values *)
module HElemProj1_(N: Newtype1) = struct
  let z HNil = HNil
  let s n (HCons (x, xs)) = HCons (N.prj x, n xs)
  let project n hs = n hs
end

module TestMapProj = struct
  module Identity = Newtype1(struct type 'a t = 'a end)

  let id = { inj = Identity.inj }

  let hlist = HCons ("1", HCons (2, HCons (3.0, HNil)))

  let funs = Repeat.(s (s (s z))) id

  let mapped = HListMap.(map (s (s (s z))) funs hlist)

  module P = HElemProj1(Identity)
  module P_ = HElemProj1_(Identity)

  (* Proof and numeral version are essentially the same *)
  let proj = P.(project (Step (Step (Step Base))) mapped)
  let proj_ = P_.(project (s (s (s z))) mapped)
end




(* a slot exposing a generative effect, just as in our framework *)
module type SLOT = sig
  type t
  effect Push: t -> unit
  val memory: t list
end
type 'a slot = (module SLOT with type t = 'a)
type slot_ex = (module SLOT)

let mkSlot (type s) (t: s typ) =
  (module struct
     type t = s
     effect Push: t -> unit
     let memory = []
   end: (SLOT with type t = s))

module WrapEvtSlot = ElementWiseWrap(struct type 'a wrap = 'a evt slot end)
module WrapEvt = ElementWiseWrap(struct type 'a wrap = 'a evt end)

(* A join consists of a type 'index' (hlist phantom type describing the arity and types) and
   a hlist of phantom type 'slots', which is related to 'index' by the Hlist_Match_Slot_Evt relation. *)
module type JOIN = sig
  type index  (*  a1 ... an    *)
  type joined (*  a1 evt ... an evt *)
  type slots  (*  a1 evt slot ... an evt slot *)

  (* You can construct a JOIN only if you can prove that the types are related in all of the following ways:  *)
  val p_index_joined_related: (index, joined) WrapEvt.proof
  val p_index_slot_related: (index, slots) WrapEvtSlot.proof

  (* Expose the slots in a hlist to enable position-dependent and type safe code generation. *)
  val slot_hlist: slots hlist
  (* hlist in existential form *)
  val slot_list: slot_ex list
end
type ('a, 'b, 'c) join = (module JOIN with type index = 'a and type joined = 'b and type slots = 'c)

let mkJoin (type i j s) (p_j: (i,j) WrapEvt.proof) (p_s: (i,s) WrapEvtSlot.proof) (hl: s hlist) (sl: slot_ex list) =
  (module struct
     type index = i
     type joined = j
     type slots = s
     let p_index_joined_related = p_j
     let p_index_slot_related = p_s
     let slot_hlist = hl
     let slot_list = sl
   end: (JOIN with type index = i and type joined = j and type slots = s))


(* Polyvariadic join construction, an application of the techniques in http://okmij.org/ftp/Computation/extra-polymorphism.html#poly-var,
   and Daniel Fridlender and Mia Indrika: Do We Need Dependent Types? J. Functional Programming, 2000. *)
module PolyJoin = struct
let z k = k (mkJoin WrapEvt.Base WrapEvtSlot.Base HNil [])
let s (type a b c) n k x = n (fun (v: (a,b,c) join) ->
                             let module J = (val v) in
                             let element_typ = (tparam_of (witness x)) in
                             let event_typ = evt_typ element_typ in
                             let slot = (mkSlot event_typ) in
                             let slot_typ = witness slot in
                             let proof_joined = WrapEvt.Step (element_typ, event_typ, J.p_index_joined_related) in
                             let proof_slots = WrapEvtSlot.Step (element_typ, slot_typ, J.p_index_slot_related) in
                             let slot_hlist = HCons (slot, J.slot_hlist) in
                             let slot_ex: (module SLOT) = (Obj.magic slot) in (* surprisingly, we need to cast*)
                             let slot_list = slot_ex :: J.slot_list in
                             let j = mkJoin proof_joined proof_slots slot_hlist slot_list in
                             k j)
let join n = n (fun x -> x)

(* tests *)
let zero  = join z                    (* JOIN with type index = unit and type slots = unit) *)
let once  = join (s z)     [1]        (* once: '_a r -> (module JOIN with type index = '_a * unit and type slots = '_a evt slot * unit) *)
let twice = join (s (s z)) [3] ["4"]  (* twice: '_a r -> '_b r ->
(module JOIN with type index = '_a * ('_b * unit) and type slots = '_a evt slot * ('_b evt slot * unit))  *)

(*
 let bad = once [1] [2];;
           ^^^^
Error: This function has type
       'a r ->
       (module JOIN with type index = 'a * unit and type slots =
        'a evt slot * unit)
     It is applied to too many arguments; maybe you forgot a `;'.
 *)
end

(* Notice that so far, everything is some kind of fold, using z and s. *)

module CartesianSigSpec = struct
  (* (t,u,v) proof iff t and u are hlist phantom types (i.e. nested tuple types)
     with (1) same length and (2) u_i = t_i -> v, i.e., u is a hlist phantom type of
     position dependent functions into the type v.   *)
  type (_,_,_) proof =
    | Base: (unit,unit,'c) proof
    | Step: ('a typ * ('b, 'c, 'd) proof) -> ('a * 'b, ('a -> 'd) * 'c, 'd) proof
end

module type JOINCARTESIAN = sig
  include JOIN
  (* cartesian =  ((a1 -> (a1 evt * ... * an evt) list)
                 * (a2 -> (a1 evt * ... * an evt) list)
                 * ...
                 * (an -> (a1 evt * ... * an evt) list)) *)
  type cartesian
  val p_cartesian_signature: (index,cartesian,joined list) CartesianSigSpec.proof
  val cartesian: cartesian hlist
end
type ('a, 'b, 'c, 'd) join_cartesian =
  (module JOINCARTESIAN with type index = 'a and type joined = 'b and type slots = 'c and type cartesian = 'd)

module PolyCartesian = struct
  let mkJoinCartesian (type i j s c) (join: (i,j,s) join)
        (p_c: (i,c,j list) CartesianSigSpec.proof) (cartesianfuns: c hlist)
    =
    let module J = (val join) in
    (module struct
       include J
       type cartesian = c
       let p_cartesian_signature = p_c
       let cartesian = cartesianfuns
     end: (JOINCARTESIAN with type index = i and type joined = j and type slots = s and type cartesian = c))

(* Key-issue: building the hlist of cartesian functions.
 *)
  (* let z = (PolyJoin.z, HListZipperMap.z)
   * let s (np,nzm) = (PolyJoin.s np, HListZipperMap.s nzm)
   * let make n =
   *   let module PJ = PolyJoin in
   *   let module ZM = HListZipperMap in
   *   let pj_n = fst n in
   *   let zm_n = snd n in
   *   PJ.join pj_n (fun join_mod ->
   *       let module J = (val join_mod) in
   *
   *       let cartesian_fun
   *
   *       (\* would like to express that cartesian_fun is polymorphic over all zippers of a type.  *\)
   *       let cartesian_fun (type a b c) (zipper: a hlist * (b slot * c) hlist) =
   *
   *       in (\* TODO: how to define this hassle-free *\)
   *       (\* let cartesian = ZM.map_zippers zm_n J.slots ??? in  (\\* TODO: how do we supply the functions? *\\) *\)
   *       (\* let p_cartesian_sig =    in *\) (\* TODO how to construct the proof from what we have? *\)
   *       (\* mkJoinCartesian join_mod p_cartesian_sig *\)
   *       failwith "not implemented, bro"
   *     ) *)


end

(* next step: cartesian impl *)
(* DSL *)
(* extensions *)
