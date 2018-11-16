#use "topfind";;
#mod_use "utility.ml";;
#mod_use "count.ml";;
#mod_use "delimcont.ml";;
#mod_use "async.ml";;


(* Lightweight HLists *)

module type hlist = sig
  type 'a el                            (* element type *)
  type _ hlist =
    | Z : unit hlist
    | S : 'a el * 'b hlist -> ('a * 'b) hlist
end

module HList(E: sig type 'a t end) = struct
  type 'a el = 'a E.t
  type _ hlist =
    | Z : unit hlist
    | S : 'a el * 'b hlist -> ('a * 'b) hlist

  let nil = Z
  let cons h t = S (h,t)
end

(* Just a closure that somehow obtains the value of the type 'a *)
module Drefs = HList(struct type 'a t = unit -> 'a end)

let d1 = Drefs.(cons (fun () -> 1) nil)
(*
val d1 : (int * unit) Drefs.hlist = Drefs.S (<fun>, Drefs.Z)
*)

let d2 = Drefs.(cons (fun () -> true) d1)

let d3 = Drefs.(cons (fun () -> "abc") d2)
(*
val d3 : (string * (bool * (int * unit))) Drefs.hlist =
  Drefs.S (<fun>, Drefs.S (<fun>, Drefs.S (<fun>, Drefs.Z)))
*)

(* a pointer into an HList *)
type (_,_) ptr =
  | Here : ('a,'a*_) ptr
  | Next : ('a,'r) ptr -> ('a,_*'r) ptr

let n0 = Here
let n1 = Next n0
let n2 = Next n1

module HListP(H: hlist) = struct
  include H

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

module DrefsP = HListP(Drefs)

let _ = DrefsP.proj n0 d3 ()
(* - : string = "abc" *)

let _ = DrefsP.proj n2 d3 ()
(* - : int = 1 *)

(*
let _ = DrefsP.proj n2 d1
Characters 16-18:
  let _ = proj n2 d1
                  ^^
Error: This expression has type (int * unit) drefs
       but an expression was expected of type (int * ('a * ('b * 'c))) drefs
       Type unit is not compatible with type 'a * ('b * 'c)
*)

let d3' = DrefsP.rplc n1 d3 (fun () -> false)
let _ = DrefsP.proj n0 d3' ()
let _ = DrefsP.proj n1 d3' ()
let _ = DrefsP.proj n2 d3' ()

(*
let d3' = DrefsP.rplc n1 d3 (fun () -> 2)
Characters 32-33:
  let d3' = rplc n1 d3 (fun () -> 2)
                                  ^
Error: This expression has type int but an expression was expected of type
         bool
*)

(* HList of lists *)

module HLL = HList(struct type 'a t = 'a list end)

let test_list = HLL.(cons [1;2;3] @@
                     cons ["A";"B"] @@ cons [0.3;0.2;0.1] @@ nil)
(*
val test_list : (int * (string * (float * unit))) HLL.hlist =
  HLL.S ([1; 2; 3], HLL.S (["A"; "B"], HLL.S ([0.3; 0.2; 0.1], HLL.Z)))
*)

(* Compute the cartesian product of HLL  *)

let rec cart : type w r. r HLL.hlist -> (r -> w list) -> w list = fun h f ->
  let open HLL in
  match h with
  | Z -> f ()
  | S (ls,h) ->
      List.concat @@ List.map (fun x -> cart h (fun xs -> f (x,xs))) ls

let _ =
    cart test_list (fun x -> [x])
(*
- : (int * (string * (float * unit))) list =
[(1, ("A", (0.3, ()))); (1, ("A", (0.2, ()))); (1, ("A", (0.1, ())));
 (1, ("B", (0.3, ()))); (1, ("B", (0.2, ()))); (1, ("B", (0.1, ())));
 (2, ("A", (0.3, ()))); (2, ("A", (0.2, ()))); (2, ("A", (0.1, ())));
 (2, ("B", (0.3, ()))); (2, ("B", (0.2, ()))); (2, ("B", (0.1, ())));
 (3, ("A", (0.3, ()))); (3, ("A", (0.2, ()))); (3, ("A", (0.1, ())));
 (3, ("B", (0.3, ()))); (3, ("B", (0.2, ()))); (3, ("B", (0.1, ())))]
*)

(* ===================== Oliver's stuff ===================== *)

(* Utility functions for generating and composing effect handlers. *)

(* TODO backport to handlers.ml *)
module Handlers = struct
  (* binary handler composition *)
  let (|+|) h1 h2 action = h1 (fun () -> h2 action)
  (* compose given list of handlers to a single handler.  *)
  let comp hs = List.fold_right (|+|) hs (fun action -> action ())
  (* single handler application *)
  let with_h h action = h action
  (* multiple handler application  *)
  let with_hs hs action = with_h (comp hs) action
  (* apply handler-generating function to a list of things and compose *)
  let gen list f = comp (List.mapi f list)
end

(* Emulates type constructors in our framework *)
type 'a react = 'a list
type 'a evt = 'a
module Reacts = struct
  include HList(struct type 'a t = 'a react end)
  let rec eat f = function (* TODO needs to change to proper stream type *)
    | [] -> ()
    | x :: xs -> (f x); eat f xs
end
module Events = HList(struct type 'a t = 'a evt end)

(* a slot exposing a generative effect, just as in our framework *)
module type SLOT = sig
  type t
  effect Push: t -> unit
  val push: t -> unit
  effect GetMail: t list (* TODO: change to the real mailbox type *)
  val getMail: unit -> t list
  effect SetMail: t list -> unit
  val setMail: t list -> unit
end
type 'a slot = (module SLOT with type t = 'a)
type slot_ex = (module SLOT)
(* Create a slot instance from a value witnessing the type. *)
let mkSlot (type s) (t: s) =
  (module struct
     type t = s
     effect Push: t -> unit
     let push v = perform (Push v)
     effect GetMail: t list
     let getMail () = perform GetMail
     effect SetMail: t list -> unit
     let setMail v = perform (SetMail v)
   end: (SLOT with type t = s))

module ListDrefs = struct
  include HList(struct type 'a t = unit -> 'a list end)
  let rec force: type a. a hlist -> a HLL.hlist =
    function
    | Z -> HLL.nil
    | S (thunk,ts) -> HLL.cons (thunk ()) (force ts)
end

module Slots = struct
  include HList(struct type 'a t = 'a slot end)
  (* Forget the concrete types of the slot hlist for uniform processing. *)
  let rec abstract: type a. a hlist -> slot_ex list =
    function
    | Z -> []
    | S (slot,hs) -> (Obj.magic slot) :: (abstract hs) (* It's weird that a cast is required in this scenario. *)

  (* Wrap effect thunks in list drefs. *)
  let rec mailboxes: type a. a hlist -> a ListDrefs.hlist =
    function
    | Z -> ListDrefs.nil
    | S (slot,hs) ->
       let module S = (val slot) in
       ListDrefs.cons (S.getMail) (mailboxes hs)
end

(* We assume this will be generated from user query. *)
module type JOIN = sig
  type index
  val slots: index Slots.hlist
end

(* Arity generic join implementation. *)
module JoinShape(J: JOIN) = struct
  include J
  let mboxes = Slots.mailboxes slots
  let slot_list = Slots.abstract slots

  effect Trigger: index -> unit
  let trigger tuple = perform (Trigger tuple)

  effect VRestriction: (unit -> 'a) -> 'a

  (* Handles the ambient mailbox state for each slot. *)
  (* Projecting to a uniformly-typed list of abstract SLOT modules makes it easier to generate handlers of
     generative effects.  *)
  let memory = Handlers.gen slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   let mem: S.t list ref = ref [] in
                   (fun action ->
                     try action () with
                     | effect (S.GetMail) k -> continue k !mem
                     | effect (S.SetMail l) k -> mem := l; continue k ()
                   ))

  (* Default behavior: enqueue each observed event notification in the corresponding mailbox. *)
  let forAll = Handlers.gen slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        S.(setMail (x :: (getMail ())));
                        continue k (S.push x)))

  (* Implements the generic cartesian product.  *)
  let reify = Handlers.gen slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        let mail = begin
                            try ListDrefs.force mboxes with
                            | effect (S.GetMail) k -> continue k [x]
                          end
                        in
                        List.iter (trigger) (cart mail (fun x -> [x])); (* TODO put real impl here*)
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
         (fun () -> Reacts.eat (S.push) r) :: (next rs))
  in (fun slots ->
      let mk_thunks = thunk_list slots in
      fun rs () -> Async.interleaved (Array.of_list (mk_thunks rs))) (* TODO: generate the array right away *)




(* let mkJoin (type i j s)
 *       (p_j: (i,j) WrapEvt.proof)
 *       (p_s: (i,s) WrapEvtSlot.proof) =
 *   (module struct
 *      type index = i
 *      type joined = j
 *      type slots = s
 *      let p_index_joined_rel = p_j
 *      let p_index_slot_rel = p_s
 *    end: (JOIN with type index = i and type joined = j and type slots = s)) *)


(* module PolyJoin = struct (\* TODO should we have an uncurried variant? *\)
 *   let z k = k (mkJoin WrapEvt.Base WrapEvtSlot.Base)
 *
 *   let s (type a b c) n k element_typ =
 *     n (fun (v: (a, b, c) join) ->
 *         let module J = (val v) in
 *         let event_typ = evt_typ element_typ in
 *         let slot_typ = slot_typ event_typ in
 *         let proof_joined = WrapEvt.Step (element_typ, event_typ, J.p_index_joined_rel) in
 *         let proof_slots = WrapEvtSlot.Step (element_typ, slot_typ, J.p_index_slot_rel) in
 *         let j = mkJoin proof_joined proof_slots in
 *         k j)
 *
 *   let mk n = n (fun x -> x)
 *
 *   (\* tests *\)
 *   let zero   = mk z
 *   let once   = mk (s z) (witness "1")
 *   let twice  = mk (s (s z)) (witness "1") (witness 2)
 *   let thrice = mk (s (s (s z))) (witness "1") (witness 2) (witness 3.0)
 *
 * (\*
 *  let bad = once (witness 1) (witness 2);;
 *            ^^^^ *\)
 * end *)

module Tests = struct
  let s1 = mkSlot 0
  let s2 = mkSlot "0"
  let s3 = mkSlot 0.0
  let s4 = mkSlot 'c'
  let slots3 = Slots.(cons s1 @@ cons s2 @@ cons s3 @@ nil)
  let slots4 = Slots.(cons s4 @@ slots3)
  let list0 = ['x';'y';'z']
  let list1 = [1;2]
  let list2 = ["A";"B"]
  let list3 = [0.3;0.2;0.1]
  let reacts3 = Reacts.(cons list1 @@ cons list2 @@ cons list3 nil)
  let reacts4 = Reacts.(cons list0 reacts3)
  let interleave3 = interleaved_bind slots3 reacts3
  let interleave4 = interleaved_bind slots4 reacts4

  (* These could be generated with appropriate Fridlender & Indrika numerals. *)
  module J3 =
    (struct
      type index = (int * (string * (float * unit)))
      let slots = slots3
     end: (JOIN with type index = (int * (string * (float * unit)))))

  module J4 =
    (struct
      type index = char * (int * (string * (float * unit)))
      let slots = slots4
     end: (JOIN with type index = char * (int * (string * (float * unit)))))

  module Join3 = JoinShape(J3)
  module Join4 = JoinShape(J4)

  let show3 action =
    try action () with
    | effect (Join3.Trigger (i, (s, (f, ())))) k ->
       continue k (Printf.printf "(%d,%s,%2.1f)\n" i s f)

  let show4 action =
    try action () with
    | effect (Join4.Trigger (c, (i, (s, (f, ()))))) k ->
       continue k (Printf.printf "(%c,%d,%s,%2.1f)\n" c i s f)

  let test_join3 () =
    let open Handlers in
    ((Async.run) |+| show3 |+| Join3.run) interleave3

  let test_join4 () =
    let open Handlers in
    ((Async.run) |+| show4 |+| Join4.run) interleave4
end
