open Prelude
open Slot
open Core

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

let correlate (type a) n =
  let open Handlers in
  n (fun ((slots,reacts): (a Slots.hlist * a Reacts.hlist)) ->
      let module JSig = (val mkJoinSig slots) in
      let streams = interleaved_bind slots reacts in
      let module JoinN = JoinShape(JSig) in
      (Async.run |+| JoinN.run) streams)

(* TODO: Could we have an applicative syntax? *)
