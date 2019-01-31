open Prelude
open Slot
open Yieldfail
open Core
open Symantics


(* We utilize the Fridlender & Indrika construction for variadic join specifications.
   rout = correlate <n> r1 ... rn
           (fun x1 ... xn -> where p yield e)
*)

let mkJoinSig: type a. a Slots.hlist -> a join_sig =
  (fun slots ->
    (module struct
       type index = a
       effect Trigger: a Events.hlist -> unit
       let slots = slots
     end: (JOIN with type index = a)))


let z k = k (Slots.nil, Reacts.nil)
let s n k react =
  let open Types in
  let elem_typ: type a. a evt r typ -> a typ = (fun Typ -> Typ) in
  n (fun (slots, reacts) ->
      let slot = mk_slot (elem_typ (witness react)) in
      k (Slots.(cons slot slots), Reacts.(cons react reacts)))

let zero  () = z
let one   () = (s z)
let two   () = (s (s z))
let three () = (s (s (s z)))

(* The correlate delimiter. Optional argument with_sig: inject an external join signature module for debugging *)
let correlate (type a) n ?(with_sig: a join_sig option) =
  n (fun ((slots,reacts): (a Slots.hlist * a Reacts.hlist)) ->
      let (slots,js) = match with_sig with (* it's important to return the right slots list! *)
        | None -> (slots, mkJoinSig slots)
        | Some j -> let module J = (val j) in
                    (J.slots, j)
      in
      let module JSig = (val js) in
      let module JoinN = JoinShape(JSig) in
      let streams = interleaved_bind slots JoinN.suspensions reacts in
      (fun () -> JoinN.run streams)) (* TODO this should be done asynchronously *)

module Cartesius = struct
  open Hlists

  (* meta data*)
  type meta = Interval.time
  let merge: meta -> meta -> meta = Interval.(|@|)
  (* TODO this boilerplate should be abstracted *)
  module MCtx = HList(struct type 'a t = meta end)
  module MFold = HFOLD(MCtx)
  let merge_hlist metas =
    MFold.(fold { zero = Interval.tzero; succ = fun m next -> merge m (next ()) } metas)

  type 'a repr = 'a
  type 'a shape = 'a evt r
  type 'a el_repr = 'a repr * meta repr

  (* TODO: can we eliminate this boilerplate somehow? *)
  include StdContextRepr(struct type 'a repr = 'a type meta = Interval.time type 'a shape = 'a evt r type 'a el_repr = 'a repr * meta repr end)

  (* expressions *)
  let pair: 'a repr -> 'b repr -> ('a * 'b) repr  = fun x y -> (x,y)
  let bool: bool -> bool repr = fun b -> b

  (* patterns, next to metadata context, we pass a generative effect instance for yielding/failing *)
  type ('ctx, 'a) pat = 'ctx MCtx.hlist -> 'a yieldfail -> 'a el_repr
  let where: bool repr -> ('m, 'a) pat -> ('m, 'a) pat = fun b p meta yf -> if b then (p meta yf) else fail_mod yf
  let yield: 'a repr -> ('m, 'a) pat = fun v metas _ -> (v, (merge_hlist metas))

  (* extensions as slot-dependent restriction handlers *)
  type 'a handler = (unit -> 'a) -> 'a
  type 'ctx ext = 'ctx Slots.hlist -> unit handler
  let empty_ext: 'ctx ext = fun _ action -> action ()
  let (|++|): 'ctx ext -> 'ctx ext -> 'ctx ext = fun h h' ctx ->
    Handlers.((h ctx) |+| (h' ctx))

  let join: type s a b. (s, a) ctx -> s ext -> (a -> (s,b) pat) -> b shape repr = fun (_,ctx) ext body ->
    let open Types in
    let module M = HMAP(Ctx)(Slots) in
    let witness: ('a,'b) var -> 'a typ = fun _ -> Typ in
    let slots = M.map {M.f = fun var -> mk_slot (witness var) } ctx in
    let yield_witness: b typ = Typ in
    let yf = mk_yieldfail yield_witness in
    failwith "not implemented"
end
