open Prelude

(* A slot x represents a binding 'x from ...' inside of a correlate block.
   Each binding has specific effects attached to it (generative effects).
   *)
module type SLOT = sig
  (* The type of event values this slot binds *)
  type t
  (* Push event notification *)
  effect Push: t -> unit
  val push: t -> unit
  (* Retrieve mailbox state, mail has a lifetime counter *)
  effect GetMail: (t * (Count.t ref)) list
  val getMail: unit -> (t * (Count.t ref)) list
  (* Set mailbox state *)
  effect SetMail: (t * (Count.t ref)) list -> unit
  val setMail: (t * (Count.t ref)) list -> unit
  val stateHandler: (unit -> 'a) -> 'a
  val forAll: (unit -> 'a) -> 'a
end

module Slot(T: SomeT): (SLOT with type t = T.t) = struct
  type t = T.t
  effect Push: t -> unit
  let push v = perform (Push v)

  (* TODO it would be better to make all these definitions external to the module *)

  effect GetMail: (t * (Count.t ref)) list
  let getMail () = perform GetMail
  effect SetMail: (t * (Count.t ref)) list -> unit
  let setMail l = perform (SetMail l)

  let stateHandler action =
    let mbox: (t * (Count.t ref)) list ref = ref [] in (* TODO avoid mutability *)
    try action () with
    | effect GetMail k -> continue k !mbox
    | effect (SetMail l) k -> mbox := l; continue k ()

  let forAll action =
    try action () with
    | effect (Push x) k ->
       setMail ((x,(ref Count.Inf)) :: (getMail ()));
       continue k (push x)
end

type slots = (module SLOT) array
