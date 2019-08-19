(* Represents a stateful capability value for programmatic suspension/resumption.
   This corresponds to a play_pause handler in the paper.
 *)
type t = { play: unit -> unit; pause: unit -> unit; state: unit -> bool; guard: 'a.(unit -> 'a) -> 'a }
let mk () =
  let state = ref true in
  let cont = ref None in
  let st () = !state in
  let pause () = state := false in
  let play () =
    match !state with
    | true -> ()
    | false ->
       match !cont with
       | None -> state := true (* failwith "resume called with no suspension" *)
       | Some thunk -> cont := None; state := true; thunk ()
  in
  let guard k =
    match !state with
    | true -> k ()
    | false ->
       let _: unit = Delimcont.shift (fun cb -> cont := Some cb) in
       k ()
  in
  { play = play; pause = pause; guard = guard; state = st}
