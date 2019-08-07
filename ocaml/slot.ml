open Prelude

(* A slot x represents a binding 'x from ...' inside of a correlate block.
   Each binding has specific effects attached to it (generative effects).
   *)
module type SLOT = sig
  type t
  effect Push: t evt -> unit
  val push: t evt -> unit
  effect GetMail: t mailbox
  val getMail: unit -> t mailbox
  effect SetMail: t mailbox -> unit
  val setMail: t mailbox -> unit
end
type 'a slot = (module SLOT with type t = 'a)
type slot_ex = (module SLOT)
let mk_slot: type a. unit -> a slot = fun () ->
  (module struct
     type t = a
     effect Push: t evt -> unit
     let push v = perform (Push v)
     effect GetMail: t mailbox
     let getMail () = perform GetMail
     effect SetMail: t mailbox -> unit
     let setMail v = perform (SetMail v)
   end: (SLOT with type t = a))

let abstract: type a. a slot -> slot_ex =
  fun slot -> Obj.magic slot

(* Create a slot instance from a value witnessing the type. *)
let mkSlot: type t. t -> t slot = fun _ -> mk_slot ()

let getMail_of: type a. a slot -> unit -> a mailbox = fun s ->
  let module S = (val s) in
  (S.getMail)

let setMail_of: type a. a slot -> a mailbox -> unit = fun s ->
  let module S = (val s) in
  (S.setMail)

let push_of: type a. a slot -> a evt -> unit = fun s ->
  let module S = (val s) in
  (S.push)
