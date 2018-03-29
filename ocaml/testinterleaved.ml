open Utility
open Handlers
open Data
open Reactive

let streams = [|liftArray [|"1";"2";"3";"4"|];
                liftArray [|"a";"b";"c";"d"|];
                liftArray [|"H";"I";"J";"K"|]|]

module type SLOT = sig
  type t
      effect Shout: t -> unit
  val shout: t -> unit
end

module Slot(T: SomeT): (SLOT with type t = T.t) = struct
  type t = T.t
      effect Shout: t -> unit
  let shout x = perform (Shout x)
end

(* (\* decorative solution *\)
 * module SuspendableSlot(S: SLOT): (SLOT with type t = (unit -> unit) * S.t) =
 * struct
 *   (\* Offer the resumption to the outer context *\)
 *   type t = (unit -> unit) * S.t
 *   effect Shout: t -> unit
 *   let shout x = perform (Shout x)
 *   let suspendable thunk =
 *     try thunk () with
 *     | effect (S.Shout x) k ->
 *       let resume () = continue k () in
 *       shout (resume, x)
 * end *)

module type SUSPEND = sig
  val pause: unit -> unit
  val play: unit -> unit
  val suspendable: (unit -> unit) -> unit
end

(* stateful solution *)
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
    | effect (S.Shout x) k ->
      S.shout x;
      match !state with
      | Run -> continue k ()
      | Pause ->
        let _: unit = Delimcont.shift (fun cb -> cont := Some cb) in
        continue k ()
end

effect Shout: (int * string) -> unit
let shout i s = perform (Shout (i,s))

effect Get: int
let get () = perform Get

effect Set: int -> unit
let set v = perform (Set v)

(* Outer (=shared) state context, which should remain synchronized between each interleaved strand *)
let with_state body =
  match body () with
  | x -> (fun _ -> x)
  | effect Get k -> (fun (st: int) -> continue k st st)
  | effect (Set x) k -> (fun _ -> continue k () x)

(* If interleaved works correctly, then the outputs should have a consecutive line numbering,
   starting from 1 and incremented by 1 each line.*)
let testVanilla () =
  let thunks = Array.mapi (fun i stream () -> eat (shout i) stream) streams in
  let body () =
    begin
      try Async.interleaved thunks with
      | effect (Shout (_,s)) k ->
        set (get () + 1);
        print_int (get ());
        print_string ": ";
        println s;
        continue k ()
    end
  in
  Async.run (fun () ->
      (with_state body 0))

module type TSLOT = SLOT with type t = string

let testSuspendable () =
  let module S0 = (Slot(struct type t = string end): TSLOT) in
  let module S1 = (Slot(struct type t = string end): TSLOT) in
  let module S2 = (Slot(struct type t = string end): TSLOT) in
  let module Suspend = SuspendSlot(S2) in
  let slots: (module TSLOT) array = [|(module S0);(module S1);(module S2)|] in
  let thunks = Array.mapi (fun i stream ->
      let module S = (val Array.get slots i) in
      let iter = (fun () -> eat (S.shout) stream) in
      if (i = 2) then
        (fun () -> Suspend.suspendable iter)
      else iter) streams in
  let state = ref 0 in
  let handle_shouts = (Array.mapi (fun i (slot : (module TSLOT)) ->
      let module S = (val slot) in
      (fun thunk ->
         try thunk () with
         | effect (S.Shout s) k ->
           println (Printf.sprintf "shout!%d" i);
           if (i == 2 && !state == 0) then begin
             println "suspending strand 2";
             state := 1;
             Suspend.pause ()
           end
           else ();
           set (get () + 1);
           print_int (get ());
           print_string ": ";
           println s;
           if (get ()) > 8 && !state == 1 then begin
             state := 2;
             println "reviving strand 2";
             Suspend.play ();
             println "and continuing immediately";
           end
           else ();
           continue k ())) slots)
  in
  let body = with_ha handle_shouts (fun () -> Async.interleaved thunks) in
  Async.run (fun () -> (with_state body 0))

let _ = testVanilla ()

let _ = testSuspendable ()
