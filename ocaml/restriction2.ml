open Prelude
open Hlists
open HPointers
open Slot
open Core2
open Utility
open Suspension

module SlotsPtr = HPointers.Proj(Slots) (* TODO should this be part of Core? *)
module SuspensionsPtr = HPointers.Proj(Suspensions)


(* TODO: this should move *)
type 'a handler = (unit -> 'a) -> 'a
type ('c,'a) chandler = 'c Slots.hlist -> 'c Suspensions.hlist -> 'a handler
let (|++|):  type c a. (c, a) chandler -> (c, a) chandler -> (c, a) chandler =
  (fun h1 h2 ctx susp -> (h1 ctx susp) |+| (h2 ctx susp))
let id_handler action = action ()

(* Important: we want the handlers to be *indexed* by the generative effects (context so to speak).
 That means, we have to change the design slightly, generating and composing
 context-accepting functions producing effect handlers! *)

let most_recently: type ctx i a. (i,ctx) ptr -> (ctx,a) chandler =
  (fun ptr slots _ ->
    (* TODO: we could further decouple the access logic from the impl *)
    let module S = (val (SlotsPtr.proj (ptr ()) slots)) in
    (fun action ->
      try action () with
      | effect (S.Push x) k ->
         (* Note: in contrast to paper, this version preserves the
            life counter assinged by the layers below.  *)
         let entry = List.find (fun (y,_) -> y = x) (S.getMail ()) in
         S.setMail (entry :: []);
         continue k (S.push x)))

let affinely: type ctx i a. int -> (i,ctx) ptr -> (ctx,a) chandler =
  (fun n ptr slots _ ->
    let module S = (val (SlotsPtr.proj (ptr ()) slots)) in
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
let aligning: type ctx xs a. (xs,ctx) mptr -> (ctx,a) chandler = (* TODO the chandler type should carry the suspension context *)
  (fun ptrs ctx suspensions ->
    let suspensions_ctx = SuspensionsPtr.mproj (ptrs ()) suspensions in
    let ctx = SlotsPtr.mproj (ptrs ()) ctx in
    let module Cells = HList(struct type 'a t = 'a evt option ref end) in
    let module SyncState = HZIP(Slots)(Cells) in
    let sync_state = (* TODO: it might be more clever to have suspension accept callbacks for resumption. *)
      let module M = HMAP(Slots)(SyncState) in
      M.map {M.f = fun s -> (s,ref None)} ctx
    in
    let reset_sync_state =
      let module M = HFOREACH(SyncState) in
      fun () -> M.foreach {M.f = fun (_,x) -> x := None} sync_state
    in
    let check_sync =
      let module F = HFOLD(SyncState) in
      let is_defined = function None -> false | _ -> true in
      fun () -> F.fold {F.zero = true; F.succ = (fun (_,x) rest -> (is_defined !x) && (rest ()))} sync_state
    in
    let push (type a) (s: a slot) x =
      let module S = (val s) in S.push x
    in
    let push_all =
      let module F = HFOLD(SyncState) in
      F.fold {F.zero = (fun () -> ());
              F.succ = (fun (s,st) rest ->
                let next = rest () in
                match !st with
                | Some x -> (fun () -> push s x; next ())
                | _ -> failwith "aligning: illegal sync state in push_all"
        )} sync_state
    in
    let play_all =
      let module M = HFOREACH(Suspensions) in
      fun () -> M.foreach {M.f = fun s -> s.play ()} suspensions_ctx
    in
    let set st evt = match !st with
      | None -> st := Some evt
      | _ -> failwith "aligning: strand was already observed"
    in
    let try_release k =
      if check_sync () then
        begin
          reset_sync_state ();
          push_all ();
          play_all ();
          k ()
        end
      else k ()
    in
    let rec gen_handler: type c. c Suspensions.hlist -> c SyncState.hlist -> a handler = fun ss sync ->
      match ss, sync with
      | Suspensions.Z, SyncState.Z -> id_handler
      | Suspensions.(S (s,ss)), SyncState.(S ((slot,st),sts)) ->
         let module S = (val slot) in
         let handler action =
           try action () with
           | effect (S.Push x) k ->
              s.pause (); set st x; try_release (continue k)
         in
         handler |+| (gen_handler ss sts)
    in
    gen_handler suspensions_ctx sync_state)

(* There are interesting use cases for working with sets of pointers (apart from restrictions that
need to refer to more than one slot), e.g., bulk application of a given restriction to multiple
positions. *)
