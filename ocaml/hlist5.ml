#use "topfind";;
#mod_use "utility.ml";;
#mod_use "handlers.ml";;
#mod_use "count.ml";;

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


(* emulate the type constructors in our framework *)
type 'a r = 'a list
type 'a evt = 'a

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
  let rec abstract: type a. a hlist -> slot_ex list =
    function
    | Z -> []
    | S (slot,hs) -> (Obj.magic slot) :: (abstract hs)

  let rec mailboxes: type a. a hlist -> a ListDrefs.hlist =
    function
    | Z -> ListDrefs.nil
    | S (slot,hs) ->
       let module S = (val slot) in
       ListDrefs.cons (S.getMail) (mailboxes hs)
end

module Tuple = HList(struct type 'a t = 'a evt end)

(* We assume this will be generated from user query. *)
module type JOIN = sig
  type index
  val slots: index Slots.hlist
  val show: index -> string
end

(* module Handlers = struct
 *   (\* compose given list of handlers to a single handler. *\)
 *   let hcomp hs =
 *     let comp h hres = (fun action -> h (fun () -> hres action)) in
 *     List.fold_right comp hs (fun action -> action ())
 *
 *   (\* apply handler-generating function to a list of things and compose *\)
 *   let gen list f = hcomp (List.mapi f list)
 * end *)

module JoinImpl(J: JOIN) = struct
  include J
  let mboxes = Slots.mailboxes slots
  let slot_list = Slots.abstract slots

  effect Trigger: index Tuple.hlist -> unit
  let trigger v = perform (Trigger v)

  let memory = Handlers.gen_handlers slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   let mem: S.t list ref = ref [] in
                   (fun action ->
                     try action () with
                     | effect (S.GetMail) k -> continue k !mem
                     | effect (S.SetMail l) k -> mem := l; continue k ()
                   ))

  let forAll = Handlers.gen_handlers slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        S.(setMail (x :: (getMail ())));
                        continue k (S.push x)))

  let reify = Handlers.gen_handlers slot_list (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        let mail = begin
                            try ListDrefs.force mboxes with
                            | effect (S.GetMail) k -> continue k [x]
                          end
                        in
                        cart mail (fun x -> Printf.printf "%s\n" (show x); [x]); (* TODO: put real impl here. *)
                        continue k ()))
  let run action =
    Handlers.with_h [memory;reify;forAll] action
end

(* tests *)
let s1 = mkSlot 0
let s2 = mkSlot "0"
let s3 = mkSlot 0.0

module J3 =
  (struct
    type index = (int * (string * (float * unit)))
    let show: index -> string = fun (i, (s, (f, ()))) -> Printf.sprintf "(%d,%s,%2.1f)" i s f
    let slots = Slots.(cons s1 @@ cons s2 @@ cons s3 @@ nil)
   end: (JOIN with type index = (int * (string * (float * unit)))))

module Join3 = JoinImpl(J3)

let test_join3 =
  let module S1 = (val s1) in
  let module S2 = (val s2) in
  let module S3 = (val s3) in
  Join3.run (fun () ->
      S1.push 1;
      S2.push "A";
      S3.push 0.3;
      S1.push 2;
      S2.push "B";
      S3.push 0.2;
      S1.push 3;
      S3.push 0.1)
