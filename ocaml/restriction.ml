open Prelude
open Hlists
open Slot
open Core


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

module SlotsPtr = HListPtr(Slots) (* TODO should this be part of Core? *)


(* Important: we want the handlers to be *indexed* by the generative effects (context so to speak).
 That means, we have to change the design slightly, generating and composing
 context-accepting functions producing effect handlers! *)

(* First try the most_recent handler: *)
let most_recently: type a i b. a Slots.hlist -> (i,a) ptr -> (unit -> b) -> b =
  (fun slots ptr ->
    (* TODO: we could further decouple the access logic from the impl *)
    let module S = (val (SlotsPtr.proj ptr slots)) in
    (fun action ->
      try action () with
      | effect (S.Push x) k ->
         (* Note: in contrast to paper, this version preserves the  *)
         let entry = List.find (fun (y,_) -> y = x) (S.getMail ()) in
         S.setMail (entry :: []);
         continue k (S.push x)))


(* There are interesting use cases for working with sets of pointers (apart from restrictions that
need to refer to more than one slot), e.g., bulk application of a given restriction to multiple
positions. *)
