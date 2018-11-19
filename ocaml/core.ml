#mod_use "slot.ml";; (* TODO: put in ocamlinit *)
open Prelude
open Hlists
open Slot

module Reacts = HList(struct type 'a t = 'a evt r end)
module Events = HList(struct type 'a t = 'a evt end)
module Mailboxes = struct
  include HList(struct type 'a t = 'a mailbox end)
  let rec cart : type w r. r hlist -> (r Events.hlist -> w list) -> w list = fun h f ->
    match h with
    | Z -> f Events.nil
    | S (ls,h) ->
       List.concat @@ List.map (fun (x,lives) -> cart h (fun xs -> f Events.(cons x xs))) ls
    (* TODO: consumer must take care of lives counter somehow *)
end

(* TODO: these can be further abstracted to work with different codomain and force into different hlist type *)
module MailboxDrefs = struct
  include HList(struct type 'a t = unit -> 'a mailbox end)
  let rec force: type a. a hlist -> a Mailboxes.hlist =
    function
    | Z -> Mailboxes.nil
    | S (thunk,ts) -> Mailboxes.cons (thunk ()) (force ts)
end

module Slots = struct
  include HList(struct type 'a t = 'a slot end)
  (* Forget the concrete types of the slot hlist for uniform processing. *)
  let rec abstract: type a. a hlist -> slot_ex list =
    function
    | Z -> []
    | S (slot,hs) -> (Obj.magic slot) :: (abstract hs) (* It's weird that a cast is required in this scenario. *)

  (* Wrap effect thunks in list drefs. *)
  let rec mailboxes: type a. a hlist -> a MailboxDrefs.hlist =
    function
    | Z -> MailboxDrefs.nil
    | S (slot,hs) ->
       let module S = (val slot) in
       MailboxDrefs.cons (S.getMail) (mailboxes hs)
end

(* We assume this will be generated from user query. *)
module type JOIN = sig
  type index
  val slots: index Slots.hlist (* TODO do we really need slots here? Could it be not generated later on demand? *)
  effect Trigger: index Events.hlist -> unit
end
type 'a join_sig = (module JOIN with type index = 'a)

(* Arity generic join implementation. *)
module JoinShape(J: JOIN) = struct
  include J
  let mboxes = Slots.mailboxes slots
  let slot_list = Slots.abstract slots

  let trigger tuple = perform (Trigger tuple)

  effect VRestriction: (unit -> 'a) -> 'a

  (* Handles the ambient mailbox state for each slot. *)
  (* Projecting to a uniformly-typed list of abstract SLOT modules makes it easier to generate handlers of
     generative effects.  *)
  let memory = Handlers.gen slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   let mem: S.t mailbox ref = ref [] in
                   (fun action ->
                     try action () with
                     | effect (S.GetMail) k -> continue k !mem
                     | effect (S.SetMail l) k -> mem := l; continue k ()))

  (* Default behavior: enqueue each observed event notification in the corresponding mailbox. *)
  let forAll = Handlers.gen slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        S.(setMail (((x, (ref Count.Inf)) :: (getMail ()))));
                        continue k (S.push x)))

  (* Implements the generic cartesian product.  *)
  let reify = Handlers.gen slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        let entry = List.find (fun (y,_) -> y = x) (S.getMail ()) in
                        let mail = begin
                            try MailboxDrefs.force mboxes with
                            | effect (S.GetMail) k -> continue k [entry] (*TODO would make more sense if the life counter was offered already in the push message *)
                          end
                        in
                        List.iter (trigger) (Mailboxes.cart mail (fun x -> [x])); (* TODO put real impl here*)
                        continue k ()))

  (* To join means applying this stack of effect handlers to a computation generating push notifications.
     If we had effect types, then the join would be an elimination form of all generative push effects
     S1.Push ... Sn.Push in the slots hlist. *)
  let run action =
    Handlers.with_hs [memory;reify;forAll] action
end

(* Generates the interleaved push iterations over n reactives *)
let interleaved_bind: type a. a Slots.hlist -> a Reacts.hlist -> unit -> unit =
  let rec thunk_list: type a. a Slots.hlist -> a Reacts.hlist -> (unit -> unit) list =
    function
    | Slots.Z -> (fun Reacts.Z -> [])
    | Slots.(S (s,ss)) ->
       let module S = (val s) in
       let next = thunk_list ss in
       (fun Reacts.(S (r,rs)) ->
         (fun () -> Reactive.eat (S.push) r) :: (next rs))
  in (fun slots ->
      let mk_thunks = thunk_list slots in
      fun rs () -> Async.interleaved (Array.of_list (mk_thunks rs))) (* TODO: generate the array right away *)


module DSL = struct
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
end
