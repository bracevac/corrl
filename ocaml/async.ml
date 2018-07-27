type 'a queue = 'a Queue.t

type 'a _promise =
    Waiting of ('a,unit) continuation queue
  | Done of 'a

type 'a promise = 'a _promise ref

let liftPromise x = ref (Done x)

type 'a _channel =
    ChWaiting of (('a, unit) continuation * (('a, unit) continuation queue))
  | ChEmpty
  | ChValues of 'a * ('a queue)

type 'a channel = 'a _channel ref

let channel () = ref ChEmpty

effect Async : (unit -> 'a) -> 'a promise
let async f = perform (Async f)

effect Yield : unit
let yield () = perform Yield

effect Await : 'a promise -> 'a
let await p = perform (Await p)

effect Receive: 'a channel -> 'a
let receive chan = perform (Receive chan)

effect Emit: ('a channel * 'a) -> unit
let emit chan v = perform (Emit (chan, v))

let q = Queue.create ()
let enqueue t = Queue.push t q
let dequeue () =
  if Queue.is_empty q then ()
  else Queue.pop q ()

let run main =
  let rec schedule_next () = dequeue ()
  (* match dequeue () with
   * | Fork p f -> fork p f
   * | Continue c v -> continue c v
   * |  *)
  and fork : 'a. 'a promise -> (unit -> 'a) -> unit = fun pr main ->
    match main () with
    | v ->
      begin match !pr with
        | Waiting q ->
          pr := Done v;
          Queue.iter (fun task -> enqueue (fun () -> continue task v)) q
        | Done _ -> failwith "Promise already resolved"
      end;
      schedule_next ()

    | effect (Async f) k ->
      let p = ref (Waiting (Queue.create ())) in
      (* Let async return immediately, which allows the caller to spawn multiple computations, if desired.
         Alternative is to let Async take a collection of arguments, so that it interacts more robustly
         with different scheduling algorithms. *)
      enqueue (fun () -> fork p f);
      continue k p;

    | effect Yield k ->
      enqueue (continue k);
      schedule_next ()

    | effect (Await p) k ->
      begin match !p with
        | Waiting q -> Queue.add k q
        | Done v -> enqueue (fun () -> continue k v)
      end; schedule_next ()

    | effect (Emit (chan, v)) k ->
      begin match !chan with
        | ChWaiting (c, q) ->
          (*TODO not sure which enqueue order makes more sense*)
          enqueue (fun () -> continue c v);
          chan := if (Queue.is_empty q)
            then ChEmpty
            else ChWaiting (Queue.pop q, q)
        | ChValues (v', q) ->
          Queue.add v q;
          chan := ChValues (v', q)
        | ChEmpty ->
          chan := ChValues (v, (Queue.create ()))
      end; enqueue (continue k); schedule_next ()

    | effect (Receive chan) k ->
      begin match !chan with
        | ChEmpty -> chan := ChWaiting (k, (Queue.create ()))
        | ChWaiting (c, q) -> Queue.add k q
        | ChValues (v, q) ->
          chan := if (Queue.is_empty q)
            then ChEmpty
            else ChValues (Queue.pop q, q);
          enqueue (fun () -> continue k v)
      end; schedule_next ()
  in
  fork (ref (Waiting (Queue.create ()))) main

(* Interleave the given thunks, keeping the current context in sync.
   Careful: We assume a cooperative concurrency model, i.e., each thunk
   sufficiently often invokes async operations, at which point we may reschedule.
   The use of interleaved in corrl upholds this assumption. *)
let interleaved thunks =
  let n = Array.length thunks in
  match n with
  | 0 -> ()
  | _ ->
    let chan = channel () in
    let count = ref n in
    let terminator thunk ()=
      thunk (); count := !count - 1
    in
    let intercept_async thunk =
      try thunk () with
      | effect (Await p) k ->
        let v = Delimcont.shift (fun cb ->
            let _ = async (fun () ->
                let v = await p in
                emit chan (fun () -> cb v))
            in ())
        in continue k v
      | effect Yield k ->
        let _ = Delimcont.shift (fun cb ->
            let _ = async (fun () ->
                let _ = yield () in
                emit chan (fun () -> cb ()))
            in ())
        in continue k ()
      | effect (Emit x) k ->
        let _ = Delimcont.shift (fun cb ->
            let _ = async (fun () ->
                perform (Emit x);
                emit chan (fun () -> cb ()))
            in ())
        in continue k ()
      | effect (Receive chan') k ->
        let v = Delimcont.shift (fun cb ->
            let _ = async (fun () ->
                let v = receive chan' in
                emit chan (fun () -> cb v))
            in ())
        in continue k v
    in
    Array.iter (fun thunk ->
        Delimcont.reset (fun () ->
            (* intercept_async (terminator (suspendable thunk))  *)
            (* intercept_async (suspendable (terminator thunk))   *)
            intercept_async (terminator thunk)))
      thunks;
    while (!count > 0) do
      (receive chan) ();
    done
