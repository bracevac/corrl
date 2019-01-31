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
       List.concat @@ List.map (fun (x,_) -> cart h (fun xs -> f Events.(cons x xs))) ls
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

(* For now, we give each slot the suspension capability by default. Ideally, such capabilities should be present
   only when required by an externally supplied restriction handler. *)
module Suspensions = struct
  include HList(struct type 'a t = Suspension.t end)
  let rec to_list: type a. a hlist -> Suspension.t list = function
    | Z -> []
    | S (s,ss) -> s :: (to_list ss)
end
let mk_suspensions: type a. a Slots.hlist -> a Suspensions.hlist = fun slots ->
 let module M = HMAP(Slots)(Suspensions) in
 M.map {M.f = fun _ -> Suspension.mk ()} slots

(* We assume this will be generated from user query. *)
module type JOIN = sig
  type index
  val slots: index Slots.hlist (* TODO do we really need slots here? Could it be not generated later on demand? *)
  effect Trigger: index Events.hlist -> unit
end
type 'a join_sig = (module JOIN with type index = 'a)

(* Arity generic join implementation. *)
module JoinShape(J: JOIN) = struct
  module Signature = J
  let mboxes = Slots.mailboxes J.slots
  let slot_list = Slots.abstract J.slots
  let suspensions = mk_suspensions J.slots

  let trigger tuple = perform (J.Trigger tuple)

  (* Handles the ambient mailbox state for each slot. *)
  (* Projecting to a uniformly-typed list of abstract SLOT modules makes it easier to generate handlers of
     generative effects.  *)
  let memory () = Handlers.gen slot_list (fun _ (s: (module SLOT)) ->
                   let module S = (val s) in
                   let mem: S.t mailbox ref = ref [] in
                   (fun action ->
                     try action () with
                     | effect (S.GetMail) k -> continue k !mem
                     | effect (S.SetMail l) k -> mem := l; continue k ()))

  (* Default behavior: enqueue each observed event notification in the corresponding mailbox. *)
  let forAll () = Handlers.gen slot_list (fun _ (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        S.(setMail (((x, (ref Count.Inf)) :: (getMail ()))));
                        continue k (S.push x)))

  (* Implements the generic cartesian product.  *)
  let reify () = Handlers.gen slot_list (fun _ (s: (module SLOT)) ->
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
                        List.iter (trigger) (Mailboxes.cart mail (fun x -> [x])); (* TODO make cart consider the life counters. *)
                        continue k ()))

  (* To join means applying this stack of effect handlers to a computation generating push notifications.
     If we had effect types, then the join would be an elimination form of all generative push effects
     S1.Push ... Sn.Push in the slots hlist. *)
  let run action =
    Handlers.with_hs [(memory ());(reify ());(forAll ())] action
end

(* Generates the interleaved push iterations over n reactives *)
let interleaved_bind: type a. a Slots.hlist -> a Suspensions.hlist -> a Reacts.hlist -> unit -> unit =
  let rec thunk_list: type a. a Slots.hlist -> a Suspensions.hlist -> a Reacts.hlist -> (unit -> unit) list =
    fun slots suspensions ->
    match slots, suspensions with
    | Slots.Z, Suspensions.Z  -> (fun Reacts.Z -> [])
    | Slots.(S (s,ss)), Suspensions.(S (c,cs)) ->
       let module S = (val s) in
       let suspendable_strand r () =
         try (Reactive.eat (S.push) r) with
         | effect (S.Push x) k ->
            S.push x;
            c.guard (continue k)
       in
       let next = thunk_list ss cs in
       (fun Reacts.(S (r,rs)) ->
         (suspendable_strand r) :: (next rs))
  in (fun slots suspensions ->
      let mk_thunks = thunk_list slots suspensions in
      fun rs () -> Async.interleaved (Array.of_list (mk_thunks rs))) (* TODO: generate the array right away *)



(* TODOs: *)
(* Dataflow network
   Integrate w. new final tagless signature
   More tests for the aligning handler
   Windows (medium) *)
