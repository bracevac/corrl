open Prelude
open Hlists
open Slot
open Core
open Utility
open Suspension

module HListPtr(H: hlist) = struct (* TODO: replace HListP with this module in hlists.ml*)
  open H

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


(* How do we model access among a subset of an hlist? *)
(* It seems, the dptr abstraction might come in handy! *)

module SlotsPtr = HListPs(Slots) (* TODO should this be part of Core? *)
module SuspensionsPtr = HListPs(Suspensions)


(* TODO: this should move *)
type 'a ctx = 'a Slots.hlist
type 'a handler = (unit -> 'a) -> 'a
type ('c,'a) chandler = 'c ctx -> 'a handler
let (|++|):  type c a. (c, a) chandler -> (c, a) chandler -> (c, a) chandler =
  (fun h1 h2 ctx -> (h1 ctx) |+| (h2 ctx))
let id_handler action = action ()

(* Important: we want the handlers to be *indexed* by the generative effects (context so to speak).
 That means, we have to change the design slightly, generating and composing
 context-accepting functions producing effect handlers! *)

let most_recently: type ctx i a. (i,ctx) ptr -> (ctx,a) chandler =
  (fun ptr slots ->
    (* TODO: we could further decouple the access logic from the impl *)
    let module S = (val (SlotsPtr.proj ptr slots)) in
    (fun action ->
      try action () with
      | effect (S.Push x) k ->
         (* Note: in contrast to paper, this version preserves the
            life counter assinged by the layers below.  *)
         let entry = List.find (fun (y,_) -> y = x) (S.getMail ()) in
         S.setMail (entry :: []);
         continue k (S.push x)))

let affinely: type ctx i a. int -> (i,ctx) ptr -> (ctx,a) chandler =
  (fun n ptr slots ->
    let module S = (val (SlotsPtr.proj ptr slots)) in
    let update mbox ev cv =
      update_first (fun (y,_) -> ev = y)
        (fun (x,c) -> c := cv; (x,c))
        mbox
    in
    (fun action ->
      try action () with
      | effect (S.Push x) k ->
         S.setMail (update (S.getMail ()) x (Count.Fin n));
         continue k (S.push x)))

(* TODO: can we express requirements on shape of context, but what about the *capabilities* of each context element?
Sam's paper on holes might be the solution! Also: couldn't we model a poor man's effect type system this way?
For now, we assume that all slots have the capability of supension/resumption. *)
let aligning: type ctx xs a. (xs,ctx) Ptrs.hlist -> ctx Suspensions.hlist -> (ctx,a) chandler = (* TODO the chandler type should carry the suspension context *)
  (fun ptrs suspensions ctx ->
    let suspensions_ctx = SuspensionsPtr.proj_ptrs suspensions ptrs in
    let ctx = SlotsPtr.proj_ptrs ctx ptrs in
    let module SyncState = HList(struct type 'a t = 'a evt option ref end) in
    let sync_state = (* TODO: it might be cleverer to have suspension accept callbacks for resumption. *)
      let module M = HMAP(Slots)(SyncState) in
      M.map {M.f = fun s -> ref None} ctx
    in
    let set st evt = match !st with
      | None -> st := Some evt
      | _ -> failwith "aligning: strand was already observed"
    in
    (* let rec gen_try_release: type b c. c Slots.hlist -> c Suspensions.hlist -> c SyncState.hlist -> (unit -> b) -> b =
     *   fun ctx ss sync ->
     *   match ctx, ss, sync with
     *   | Slots.Z, Suspensions.Z, SyncState.Z -> id_handler
     *   | Slots.(S (slot, ctx)), Suspensions.(S (s,ss)), SyncState.(S (st,sts)) ->
     * in *)
    let try_release k = failwith "not implemented" in
    let rec gen_handler: type c. c Slots.hlist -> c Suspensions.hlist -> c SyncState.hlist -> a handler = fun ctx ss sync ->
      match ctx, ss, sync with
      | Slots.Z, Suspensions.Z, SyncState.Z -> id_handler
      | Slots.(S (slot, ctx)), Suspensions.(S (s,ss)), SyncState.(S (st,sts)) ->
         let module S = (val slot) in
         let handler action =
           try action () with
           | effect (S.Push x) k ->
              s.pause (); set st x; try_release (continue k)
         in
         handler |+| (gen_handler ctx ss sts)
    in
    gen_handler ctx suspensions_ctx sync_state)

(* There are interesting use cases for working with sets of pointers (apart from restrictions that
need to refer to more than one slot), e.g., bulk application of a given restriction to multiple
positions. *)
