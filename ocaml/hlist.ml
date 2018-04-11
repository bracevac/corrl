
(* Simple heterogeneous list GADT *)
type _ hlist =
  | HNil : unit hlist
  | HCons : ('a * 'b hlist) -> ('a * 'b) hlist


(* emulate the type constructors in our framework *)
type 'a r = 'a list
type 'a evt = 'a

(* to construct type witnesses and map types to other types *)
type 'a typ = Typ
let witness: 'a -> 'a typ = (fun _ -> Typ)
let evt_typ: 'a typ -> 'a evt typ = (fun _ -> Typ)
let tparam_of: 'a r typ -> 'a typ = (fun _ -> Typ)

(* a slot exposing a generative effect, just as in our framework *)
module type SLOT = sig
  type t
  effect Push: t -> unit
end
type 'a slot = (module SLOT with type t = 'a)

let mkSlot (type s) (t: s typ) =
  (module struct
     type t = s
     effect Push: t -> unit
   end: (SLOT with type t = s))

(* Implements a relation among phantom types of hlists *)
module Hlist_Match_Slot_Evt = struct
  (* (t,u) proof iff t and u are hlist phantom types (i.e. nested tuple types)
     with (1) same length and (2) u_i = t_i evt slot, i.e., u is type constructor (_ evt slot) applied to t elementwise.   *)
  type (_, _) proof =
    | Base: (unit, unit) proof
    | Step: ('a typ * 'a evt slot typ * ('b, 'c) proof) -> (('a * 'b), ('a evt slot * 'c)) proof
end

(* A join consists of a type 'index' (hlist phantom type describing the arity and types) and
   a hlist of phantom type 'slots', which is related to 'index' by the Hlist_Match_Slot_Evt relation. *)
module type JOIN = sig
  type index
  type slots
  (* You can only construct a JOIN if you can prove that the index and slots are in this relation:  *)
  val p_is_wrapped: (index, slots) Hlist_Match_Slot_Evt.proof
  (* Expose the slots in a hlist to enable position-dependent and type safe code generation. *)
  val hlist: slots hlist
end
type ('a, 'b) join = (module JOIN with type index = 'a and type slots = 'b)

let mkJoin (type i) (type s) (p: (i,s) Hlist_Match_Slot_Evt.proof) (hl: s hlist) =
  (module struct
     type index = i
     type slots = s
     let p_is_wrapped = p
     let hlist = hl
   end: (JOIN with type index = i and type slots = s))

(* Polyvariadic join construction, an application of the techniques in http://okmij.org/ftp/Computation/extra-polymorphism.html#poly-var,
   and Daniel Fridlender and Mia Indrika: Do We Need Dependent Types? J. Functional Programming, 2000. *)
let z k = k (mkJoin Hlist_Match_Slot_Evt.Base HNil)
let s (type a) (type b) n k x = n (fun (v: (a,b) join) ->
                                    let module J = (val v) in
                                    let element_typ = (tparam_of (witness x)) in
                                    let slot = (mkSlot (evt_typ element_typ)) in
                                    let slot_typ = witness slot in
                                    let proof = Hlist_Match_Slot_Evt.Step (element_typ, slot_typ, J.p_is_wrapped) in
                                    let hlist = HCons (slot, J.hlist) in
                                    let j = mkJoin proof hlist in
                                    k j)
let join n = n (fun x -> x)


(* tests *)
let zero  = join z                    (* JOIN with type index = unit and type slots = unit) *)
let once  = join (s z)     [1]        (* once: '_a r -> (module JOIN with type index = '_a * unit and type slots = '_a evt slot * unit) *)
let twice = join (s (s z)) [3] ["4"]  (* twice: '_a r -> '_b r -> (module JOIN with type index = '_a * ('_b * unit) and type slots = '_a evt slot * ('_b evt slot * unit))  *)

(*
 let bad = once [1] [2];;
           ^^^^
Error: This function has type
       'a r ->
       (module JOIN with type index = 'a * unit and type slots =
        'a evt slot * unit)
     It is applied to too many arguments; maybe you forgot a `;'.
*)
