open Prelude
open Types

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
(* Create a slot instance from a value witnessing the type. *)
let mk_slot (type s) (_: s typ) =
  (module struct
     type t = s
     effect Push: t evt -> unit
     let push v = perform (Push v)
     effect GetMail: t mailbox
     let getMail () = perform GetMail
     effect SetMail: t mailbox -> unit
     let setMail v = perform (SetMail v)
   end: (SLOT with type t = s))
let mkSlot (type s) (t: s) = mk_slot (witness t)
