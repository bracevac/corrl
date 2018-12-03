open Prelude
open Slot

module type SUSPEND = sig
  val pause: unit -> unit
  val play: unit -> unit
  val suspendable: (unit -> unit) -> unit
end

module SuspendSlot(S: SLOT): SUSPEND =
struct
  type st = Run | Pause

  let state = ref Run
  let cont: (unit -> unit) option ref = ref None

  let pause () =
    state := Pause

  let play () =
    match !state with
    | Run -> ()
    | Pause ->
      match !cont with
      | None -> failwith "resume called with no suspension"
      | Some thunk -> cont := None; state := Run; thunk ()

  let suspendable thunk =
    try thunk () with
    | effect (S.Push x) k ->
      S.push x;
      match !state with
      | Run -> continue k ()
      | Pause ->
        (* This'll ensure that the current async strand is properly captured and stored. *)
        let _: unit = Delimcont.shift (fun cb -> cont := Some cb) in
        continue k ()
end

(* Represents a stateful capability value for programmatic suspension/resumption *)
type t = { play: unit -> unit; pause: unit -> unit; guard: 'a.(unit -> 'a) -> 'a }
let mk () =
  let state = ref true in
  let cont = ref None in
  let pause () = state := false in
  let play () =
    match !state with
    | true -> ()
    | false ->
       match !cont with
       | None -> failwith "resume called with no suspension"
       | Some thunk -> cont := None; state := true; thunk ()
  in
  let guard k =
    match !state with
    | true -> k ()
    | false ->
       let _: unit = Delimcont.shift (fun cb -> cont := Some cb) in
       k ()
  in
  { play = play; pause = pause; guard = guard}
