open Prelude
open Slot
open Core

(* We utilize the Fridlender & Indrika construction for variadic join specifications.
   rout = correlate <n> r1...rn ()

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
      let streams = interleaved_bind slots reacts in
      let module JoinN = JoinShape(JSig) in
      (fun () -> JoinN.run streams)) (* TODO this should be done asynchronously *)

(* TODO: Could we have an applicative syntax? *)
