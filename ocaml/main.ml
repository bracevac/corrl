module type DELIMCONT = sig
  effect Shift: (('a -> unit) -> unit) -> 'a
  val shift: (('a -> unit) -> unit) -> 'a
  val reset: (unit -> unit) -> unit
end

module DelimCont: DELIMCONT = struct
  effect Shift: (('a -> unit) -> unit) -> 'a
  let shift k = perform (Shift k)
  let reset action =
    try action () with
    | effect (Shift f) k -> f (fun x -> continue k x)
end

module type ASYNC = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
  val liftPromise : 'a -> 'a promise
  val interleaved : (unit -> unit) list -> unit
end

module Async : ASYNC = struct

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

  let liftPromise x = ref (Done x)

  effect Async : (unit -> 'a) -> 'a promise
  let async f = perform (Async f)

  effect Yield : unit
  let yield () = perform Yield

  effect Await : 'a promise -> 'a
  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match main () with
        | v -> begin match !pr with
               | Waiting l -> pr := Done v; List.iter (fun task -> enqueue (fun () -> continue task v)) l
               | Done _ -> failwith "Promise already resolved"
               end;
               dequeue ()

        | effect (Async f) k ->
           let p = ref (Waiting []) in
           enqueue (fun () -> continue k p);
           fork p f

        | effect Yield k ->
            enqueue (continue k);
            dequeue ()

        | effect (Await p) k ->
            begin match !p with
            | Done v -> continue k v
            | Waiting l ->
               p := Waiting (l @ [k]);
               dequeue ()
            end
    in
    fork (ref (Waiting [])) main

  let interleaved thunks = failwith "interleaved not implemented" (*TODO*)
end


(* Time models are monoids over some time representation *)
module type TIMEMODEL = sig
  type time
  val ( |@| ) : time -> time -> time
  val tzero : time
end

(* An event is evidence of something that happened at a specific time *)
module type EVENT = sig
  module Time : TIMEMODEL
  type 'a evt = Ev of 'a * Time.time
end

module Event(T: TIMEMODEL): (EVENT with module Time = T) = struct
  module Time = T
  type 'a evt = Ev of 'a * T.time
end

(* Our default time model is interval-based *)
module Interval : (TIMEMODEL with type time = int * int) = struct
  type time = int * int
  let ( |@| ) (a,b) (c,d) = (min a c, max b d)
  let tzero = (max_int, min_int) (* representation of empty interval *)
end

module Evt = Event(Interval)

(* Event streams *)
module Reactive = struct
  type 'a react = RNil | RCons of 'a * ('a react Async.promise)
  type 'a r = 'a react Async.promise

  let toR: 'a list -> 'a r = fun l ->
    let rec _toR =
    function
    | [] -> RNil
    | x::xs -> RCons (x, (Async.liftPromise (_toR xs)))
    in Async.liftPromise (_toR l)

  let liftArray: 'a array -> 'a r = fun a ->
    Async.liftPromise (Array.fold_right (fun x -> fun r -> RCons (x, (Async.liftPromise r))) a RNil)

  let rec eat f stream =
    match Async.await stream with
    | RCons (hd, tl) -> (f hd); eat f tl
    | RNil -> ()
end

open Evt
open Evt.Time
open Reactive

module type SomeT = sig
  type t
end

module type SINGLEWORLD = sig
  type t
  effect Yield  : t -> unit
  effect Cancel : unit

  val yield: t -> unit
  val cancel: unit -> unit

  val handler: (unit -> 'a) -> t option
end

module SingleWorld(T: SomeT): (SINGLEWORLD with type t = T.t) = struct
  type t = T.t
  effect Yield  : t -> unit
  effect Cancel : unit

  let yield x = perform (Yield x)
  let cancel () = perform Cancel

  let handler action =
    match action () with
    | x -> None
    | effect (Yield v) _ -> Some v
    | effect Cancel _ -> None
end

module ManyWorlds = struct
  effect Fork : bool
  let fork () = perform Fork

  (* TODO can we have this mutability-free? Seems we need shallow handlers. *)
  let run onDone worlds action =
    let next () =
      begin match !worlds with
      | [] -> ()
      | x::xs -> worlds := xs; x ()
      end in
    begin match action () with
    | x -> onDone(x); next () (* TODO should this be interleaved? *)
    | effect Fork k ->
       let k2 = Obj.clone_continuation k in (* This is where we need multi-shot continuations  *)
       let choices = [(fun () -> continue k true); (fun () -> continue k2 false)] in
       worlds := !worlds @ choices; (* Swap for DFS  *)
       next ()
    end

  let handler onDone action = run onDone (ref []) action
end

let context (type a) (show: a -> string) action =
  let onDone = function
    | None -> ()
    | Some x -> print_string (show x); print_newline ()
  in
  ManyWorlds.handler onDone action

(** Models a type for lifetime counters, which is either a finite int value or infinity. *)
module Count = struct
  (* TODO prohibit negative values *)
  type t = Inf | Fin of int
  let map f = function
    | Inf -> Inf
    | Fin i -> Fin (f i)
  let flatMap f = function
    | Inf -> Inf
    | Fin i -> (f i)
  let inc n = map (fun i -> i + 1) n
  let inc_by num n = map (fun i -> i + num) n
  let dec n = map (fun i -> i - 1) n
  let dec_by num n = map (fun i -> i - num) n
  let add n m = flatMap
                  (fun i ->
                    map (fun j -> i + j) m) n
  let sub n m = flatMap
                  (fun i ->
                    map (fun j -> i - j) m) n
  let lt n m = match (n,m) with
    | (_, Inf) -> true
    | (Fin i, Fin j) -> i < j
    | _ -> false
  let lte n m = (n = m) || (lt n m)
  let gt n m = not (lte n m)
  let gte n m = (n = m) || (gt n m)
  let lt_i i n = lt (Fin i) n
  let lte_i i n = lte (Fin i) n
  let gt_i i n = gt (Fin i) n
  let gte_i i n = gte (Fin i) n
end

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

let with_h hs action =
  let comp h thnk = (fun () -> (h thnk)) in
  List.fold_right comp hs action

let rec forkEach f = function
  | [] -> ()
  | x::xs -> if (ManyWorlds.fork ()) then (f x) else (forkEach f xs)

let flatMap f l = List.concat (List.map f l)

let rec update_first p f = function
  | [] -> []
  | x::xs ->
     if (p x)
     then (f x) :: xs
     else x :: (update_first p f xs)

let inc_snd n l = List.map (fun (y,c) -> (y, (Count.add c n))) l
let dec_snd n l = List.map (fun (y,c) -> (y, (Count.sub c n))) l

module type JOIN = sig
  type input
  type joined
  type result
  val slots: slots
  effect Join: (input * (joined -> unit)) -> unit
  val join: input -> (joined -> unit) -> unit
  effect Trigger: joined -> unit
  val trigger: joined -> unit
  effect SetCont: (joined -> unit) -> unit
  val setCont: (joined -> unit) -> unit
  val assemble: (unit -> unit) -> unit (* TODO: generalize return type? *)
  val ambientState: (unit -> unit) -> unit
  val correlate :
    ?window:((unit -> unit) -> unit) ->
    ?restriction:((unit -> unit) -> unit) ->
    (unit -> unit) -> unit -> unit
end

(*lame! can we have a nice arity-abstracting join definition for any n?*)
module Join4(T: sig type t0 type t1 type t2 type t3 type result end): (JOIN with type joined = T.t0 evt * T.t1 evt * T.t2 evt * T.t3 evt
                                                                             and type input = T.t0 evt r * T.t1 evt r * T.t2 evt r * T.t3 evt r
                                                                             and type result = T.result evt)
  = struct
  module S0 = Slot(struct type t = T.t0 evt end)
  module S1 = Slot(struct type t = T.t1 evt end)
  module S2 = Slot(struct type t = T.t2 evt end)
  module S3 = Slot(struct type t = T.t3 evt end)
  type joined = S0.t * S1.t * S2.t * S3.t
  type result = T.result evt
  let slots: slots = [|(module S0);(module S1);(module S2);(module S3)|]
  type input = S0.t r * S1.t r * S2.t r * S3.t r
  effect Join: (input * (joined -> unit)) -> unit
  let join sp f = perform (Join (sp, f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    let _cart f thnk1 thnk2 thnk3 (x,xc) =
      flatMap
        (fun (y,yc) ->
          flatMap
            (fun (z,zc) ->
              flatMap
                (fun (w,wc) ->
                  let lives = [xc;yc;zc;wc] in
                  if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
                  then
                    begin
                      List.iter (fun r -> r := Count.dec !r) lives;
                      [f (x,y,z,w)]
                    end
                  else [])
                (thnk3 ()))
            (thnk2 ()))
        (thnk1 ())

    let shuffle0 p = p
    let shuffle1 (x,y,z,w) = (y,x,z,w)
    let shuffle2 (x,y,z,w) = (y,z,x,w)
    let shuffle3 (x,y,z,w) = (y,z,w,x)

    (* GC the dead events all mailboxes. *)
    let cleanup () =
      Array.iter (fun (slot : (module SLOT)) ->
          let module S = (val slot) in
          S.setMail (List.filter (fun (_, c) -> Count.lt_i 0 !c) (S.getMail ()))) slots

    let eat_all (s0,s1,s2,s3) =
      let thunks = [(fun () -> Reactive.eat S0.push s0);
                    (fun () -> Reactive.eat S1.push s1);
                    (fun () -> Reactive.eat S2.push s2);
                    (fun () -> Reactive.eat S3.push s3)] in
      List.iter (fun f -> f ()) thunks (* TODO this should be interleaved, see Async module *)
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: (S0.t * Count.t ref) -> joined list =
    (_cart shuffle0 S1.getMail S2.getMail S3.getMail)
  let cartesian1: (S1.t * Count.t ref) -> joined list =
    (_cart shuffle1 S0.getMail S2.getMail S3.getMail)
  let cartesian2: (S2.t * Count.t ref) -> joined list =
    (_cart shuffle2 S0.getMail S1.getMail S3.getMail)
  let cartesian3: (S3.t * Count.t ref) -> joined list =
    (_cart shuffle3 S0.getMail S1.getMail S2.getMail)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S0.getMail ()) in
       forkEach trigger (cartesian0 x);
       cleanup ();
       continue k ()
    | effect (S1.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S1.getMail ()) in
       forkEach trigger (cartesian1 x);
       cleanup ();
       continue k ()
    | effect (S2.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S2.getMail ()) in
       forkEach trigger (cartesian2 x);
       cleanup ();
       continue k ()
    | effect (S3.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S3.getMail ()) in
       forkEach trigger (cartesian3 x);
       cleanup ();
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState (action: unit -> unit) =
    let action =
      with_h [S0.stateHandler;
              S1.stateHandler;
              S2.stateHandler;
              S3.stateHandler] action in
    let cont: (joined -> unit) option ref = ref None in
    try action () with
    | effect (SetCont c) k -> cont := Some c; continue k ()
    | effect (Trigger res) k ->
       match !cont with
       | Some c ->
          c res
       | None -> failwith "uninitialized join continuation"

  let correlate ?(window=(fun f -> f ())) ?(restriction=(fun f -> f ())) pattern () =
    let setup () =
      try pattern () with
      | effect (Join (streams, c)) k ->
           setCont c;
           let _ = eat_all streams in (* TODO keep the promises? *)
           ()
    in
    with_h [ambientState;
            assemble;
            restriction;
            S0.forAll;
            S1.forAll;
            S2.forAll;
            S3.forAll;
            window]
      setup ()
end

module Join3(T: sig type t0 type t1 type t2 type result end): (JOIN with type joined = T.t0 evt * T.t1 evt * T.t2 evt
                                                                     and type input = T.t0 evt r * T.t1 evt r * T.t2 evt r
                                                                     and type result = T.result evt)
  = struct
  module S0 = Slot(struct type t = T.t0 evt end)
  module S1 = Slot(struct type t = T.t1 evt end)
  module S2 = Slot(struct type t = T.t2 evt end)
  type joined = S0.t * S1.t * S2.t
  type result = T.result evt
  let slots: slots = [|(module S0);(module S1);(module S2)|]
  type input = S0.t r * S1.t r * S2.t r
  effect Join: (input * (joined -> unit)) -> unit
  let join sp f = perform (Join (sp, f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    let _cart f thnk1 thnk2 (x,xc) =
      flatMap
        (fun (y,yc) ->
          flatMap
            (fun (z,zc) ->
                  let lives = [xc;yc;zc] in
                  if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
                  then
                    begin
                      List.iter (fun r -> r := Count.dec !r) lives;
                      [f (x,y,z)]
                    end
                  else [])
            (thnk2 ()))
        (thnk1 ())

    let shuffle0 p = p
    let shuffle1 (x,y,z) = (y,x,z)
    let shuffle2 (x,y,z) = (y,z,x)
    let shuffle3 (x,y,z) = (y,z,x)

    (* GC the dead events all mailboxes. *)
    let cleanup () =
      Array.iter (fun (slot : (module SLOT)) ->
          let module S = (val slot) in
          S.setMail (List.filter (fun (_, c) -> Count.lt_i 0 !c) (S.getMail ()))) slots

    let eat_all (s0,s1,s2) =
      let thunks = [(fun () -> Reactive.eat S0.push s0);
                    (fun () -> Reactive.eat S1.push s1);
                    (fun () -> Reactive.eat S2.push s2)] in
      List.iter (fun f -> f ()) thunks (* TODO this should be interleaved, see Async module *)
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: (S0.t * Count.t ref) -> joined list =
    (_cart shuffle0 S1.getMail S2.getMail)
  let cartesian1: (S1.t * Count.t ref) -> joined list =
    (_cart shuffle1 S0.getMail S2.getMail)
  let cartesian2: (S2.t * Count.t ref) -> joined list =
    (_cart shuffle2 S0.getMail S1.getMail)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S0.getMail ()) in
       forkEach trigger (cartesian0 x);
       cleanup ();
       continue k ()
    | effect (S1.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S1.getMail ()) in
       forkEach trigger (cartesian1 x);
       cleanup ();
       continue k ()
    | effect (S2.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S2.getMail ()) in
       forkEach trigger (cartesian2 x);
       cleanup ();
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState (action: unit -> unit) =
    let action =
      with_h [S0.stateHandler;
              S1.stateHandler;
              S2.stateHandler] action in
    let cont: (joined -> unit) option ref = ref None in
    try action () with
    | effect (SetCont c) k -> cont := Some c; continue k ()
    | effect (Trigger res) k ->
       match !cont with
       | Some c ->
          c res
       | None -> failwith "uninitialized join continuation"

  let correlate ?(window=(fun f -> f ())) ?(restriction=(fun f -> f ())) pattern () =
    let setup () =
      try pattern () with
      | effect (Join (streams, c)) k ->
           setCont c;
           let _ = eat_all streams in (* TODO keep the promises? *)
           ()
    in
    with_h [ambientState;
            assemble;
            restriction;
            S0.forAll;
            S1.forAll;
            S2.forAll;
            window]
      setup ()
end

module Join2(T: sig type t0 type t1 type result end): (JOIN with type joined = T.t0 evt * T.t1 evt
                                                             and type input = T.t0 evt r * T.t1 evt r
                                                             and type result = T.result evt)
  = struct
  module S0 = Slot(struct type t = T.t0 evt end)
  module S1 = Slot(struct type t = T.t1 evt end)
  type joined = S0.t * S1.t
  type result = T.result evt
  let slots: slots = [|(module S0);(module S1)|]
  type input = S0.t r * S1.t r
  effect Join: (input * (joined -> unit)) -> unit
  let join sp f = perform (Join (sp, f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    let _cart f thnk1 (x,xc) =
      flatMap
        (fun (y,yc) ->
          let lives = [xc;yc] in
          if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
          then
            begin
              List.iter (fun r -> r := Count.dec !r) lives;
              [f (x,y)]
            end
          else [])
        (thnk1 ())

    let shuffle0 p = p
    let shuffle1 (x,y) = (y,x)
    let shuffle2 (x,y) = (y,x)
    let shuffle3 (x,y) = (y,x)

    (* GC the dead events all mailboxes. *)
    let cleanup () =
      Array.iter (fun (slot : (module SLOT)) ->
          let module S = (val slot) in
          S.setMail (List.filter (fun (_, c) -> Count.lt_i 0 !c) (S.getMail ()))) slots

    let eat_all (s0,s1) =
      let thunks = [(fun () -> Reactive.eat S0.push s0);
                    (fun () -> Reactive.eat S1.push s1)] in
      List.iter (fun f -> f ()) thunks (* TODO this should be interleaved, see Async module *)
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: (S0.t * Count.t ref) -> joined list =
    (_cart shuffle0 S1.getMail)
  let cartesian1: (S1.t * Count.t ref) -> joined list =
    (_cart shuffle1 S0.getMail)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S0.getMail ()) in
       forkEach trigger (cartesian0 x);
       cleanup ();
       continue k ()
    | effect (S1.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S1.getMail ()) in
       forkEach trigger (cartesian1 x);
       cleanup ();
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState (action: unit -> unit) =
    let action =
      with_h [S0.stateHandler;
              S1.stateHandler] action in
    let cont: (joined -> unit) option ref = ref None in
    try action () with
    | effect (SetCont c) k -> cont := Some c; continue k ()
    | effect (Trigger res) k ->
       match !cont with
       | Some c ->
          c res
       | None -> failwith "uninitialized join continuation"

  let correlate ?(window=(fun f -> f ())) ?(restriction=(fun f -> f ())) pattern () =
    let setup () =
      try pattern () with
      | effect (Join (streams, c)) k ->
           setCont c;
           let _ = eat_all streams in (* TODO keep the promises? *)
           ()
    in
    with_h [ambientState;
            assemble;
            restriction;
            S0.forAll;
            S1.forAll;
            window]
      setup ()
end

module Join1(T: sig type t0 type result end): (JOIN with type joined = T.t0 evt
                                                     and type input = T.t0 evt r
                                                     and type result = T.result evt)
  = struct
  module S0 = Slot(struct type t = T.t0 evt end)
  type joined = S0.t
  type result = T.result evt
  let slots: slots = [|(module S0)|]
  type input = S0.t r
  effect Join: (input * (joined -> unit)) -> unit
  let join sp f = perform (Join (sp, f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    let _cart f (x,xc) =
      let lives = [xc] in
      if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
      then
        begin
          List.iter (fun r -> r := Count.dec !r) lives;
          [f x]
        end
      else []


    let shuffle0 p = p
    let shuffle1 (x,y) = (y,x)
    let shuffle2 (x,y) = (y,x)
    let shuffle3 (x,y) = (y,x)

    (* GC the dead events all mailboxes. *)
    let cleanup () =
      Array.iter (fun (slot : (module SLOT)) ->
          let module S = (val slot) in
          S.setMail (List.filter (fun (_, c) -> Count.lt_i 0 !c) (S.getMail ()))) slots

    let eat_all s0 =
      let thunks = [(fun () -> Reactive.eat S0.push s0)] in
      List.iter (fun f -> f ()) thunks (* TODO this should be interleaved, see Async module *)
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: (S0.t * Count.t ref) -> joined list =
    (_cart shuffle0)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       let x = List.find (fun (ev,_) -> ev = v) (S0.getMail ()) in
       forkEach trigger (cartesian0 x);
       cleanup ();
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState (action: unit -> unit) =
    let action =
      with_h [S0.stateHandler] action in
    let cont: (joined -> unit) option ref = ref None in
    try action () with
    | effect (SetCont c) k -> cont := Some c; continue k ()
    | effect (Trigger res) k ->
       match !cont with
       | Some c ->
          c res
       | None -> failwith "uninitialized join continuation"

  let correlate ?(window=(fun f -> f ())) ?(restriction=(fun f -> f ())) pattern () =
    let setup () =
      try pattern () with
      | effect (Join (streams, c)) k ->
           setCont c;
           let _ = eat_all streams in (* TODO keep the promises? *)
           ()
    in
    with_h [ambientState;
            assemble;
            restriction;
            S0.forAll;
            window]
      setup ()
end

let mostRecent (join: (module JOIN)) i action =
  let module J = (val join) in
  let module Si = (val (Array.get J.slots i)) in
  try action () with
  | effect (Si.Push v) k ->
     (* mostRecent means to forget the past *)
     Si.setMail([(v,ref (Count.Inf))]);
     continue k (Si.push v)

(** Restrict the ith slot of a join such that each event for i is associated at most n times in a product *)
let affine n (join: (module JOIN)) i action =
  let module J = (val join) in
  let module Si = (val (Array.get J.slots i)) in
  try action () with
  | effect (Si.Push v) k ->
     Si.setMail(update_first
                  (fun (x,_) -> x = v)
                  (fun (x,c) ->
                    c := (Count.Fin n);
                    (x,c))
                  (Si.getMail ()));
       continue k (Si.push v)

(* TODO: there is a nicer way to implement align, but it requires the interleaved combinator. *)

let align2 (join: (module JOIN)) action =
  let module J = (val join) in
  let _ = assert ((Array.length J.slots) = 2) in
  let module S0 = (val (Array.get J.slots 0)) in
  let module S1 = (val (Array.get J.slots 1)) in
  let tryFire () = begin
    let m0 = List.rev (S0.getMail ()) in
    let m1 = List.rev (S1.getMail ()) in
    match (m0,m1) with
    | ((ev0,_)::r0, (ev1,_)::r1) ->
       let res: J.joined = Obj.magic (ev0,ev1) in (* ugly! *)
       S0.setMail (List.rev r0);
       S1.setMail (List.rev r1);
       forkEach J.trigger [res]
    | _ -> ()
    end
  in
  try action () with
  | effect (S0.Push _) k ->
     continue k (tryFire ())
  | effect (S1.Push _) k ->
     continue k (tryFire ())

let align3 (join: (module JOIN)) action =
  let module J = (val join) in
  let _ = assert ((Array.length J.slots) = 3) in
  let module S0 = (val (Array.get J.slots 0)) in
  let module S1 = (val (Array.get J.slots 1)) in
  let module S2 = (val (Array.get J.slots 2)) in
  let tryFire () = begin
    let m0 = List.rev (S0.getMail ()) in
    let m1 = List.rev (S1.getMail ()) in
    let m2 = List.rev (S2.getMail ()) in
    match (m0,m1,m2) with
    | ((ev0,_)::r0, (ev1,_)::r1, (ev2,_)::r2) ->
       let res: J.joined = Obj.magic (ev0,ev1,ev2) in (* ugly! *)
       S0.setMail (List.rev r0);
       S1.setMail (List.rev r1);
       S2.setMail (List.rev r2);
       forkEach J.trigger [res]
    | _ -> ()
    end
  in
  try action () with
  | effect (S0.Push _) k ->
     continue k (tryFire ())
  | effect (S1.Push _) k ->
     continue k (tryFire ())
  | effect (S2.Push _) k ->
     continue k (tryFire ())

let align4 (join: (module JOIN)) action =
  let module J = (val join) in
  let _ = assert ((Array.length J.slots) = 4) in
  let module S0 = (val (Array.get J.slots 0)) in
  let module S1 = (val (Array.get J.slots 1)) in
  let module S2 = (val (Array.get J.slots 2)) in
  let module S3 = (val (Array.get J.slots 3)) in
  let tryFire () = begin
    let m0 = List.rev (S0.getMail ()) in
    let m1 = List.rev (S1.getMail ()) in
    let m2 = List.rev (S2.getMail ()) in
    let m3 = List.rev (S3.getMail ()) in
    match (m0,m1,m2,m3) with
    | ((ev0,_)::r0, (ev1,_)::r1, (ev2,_)::r2, (ev3,_)::r3) ->
       let res: J.joined = Obj.magic (ev0,ev1,ev2,ev3) in (* ugly! *)
       S0.setMail (List.rev r0);
       S1.setMail (List.rev r1);
       S2.setMail (List.rev r2);
       S3.setMail (List.rev r3);
       forkEach J.trigger [res]
    | _ -> ()
    end
  in
  try action () with
  | effect (S0.Push _) k ->
     continue k (tryFire ())
  | effect (S1.Push _) k ->
     continue k (tryFire ())
  | effect (S2.Push _) k ->
     continue k (tryFire ())
  | effect (S3.Push _) k ->
     continue k (tryFire ())

module Test = struct
  let testStreams = begin
      let e1 = Ev (0, (1,1)) in
      let e2 = Ev (2, (2,2)) in
      let e3 = Ev (4, (3,3)) in
      let e4 = Ev (6, (4,4)) in
      let s1 = toR([e1; e2; e3; e4]) in
      let e5 = Ev ("1", (5,5)) in
      let e6 = Ev ("3", (6,6)) in
      let e7 = Ev ("5", (7,7)) in
      let e8 = Ev ("7", (8,8)) in
      let s2 = toR([e5; e6; e7; e8]) in
      let e9 =  Ev (3.0, (9,9)) in
      let e10 = Ev (6.0, (10,10)) in
      let e11 = Ev (9.0, (11,11)) in
      let e12 = Ev (12.0, (12,12)) in
      let s3 = toR([e9; e10; e11; e12]) in
      let e13 = Ev (11, (8,9)) in
      let e14 = Ev (13, (9,10)) in
      let e15 = Ev (17, (10,11)) in
      let e16 = Ev (19, (11,12)) in
      let s4 = toR([e13; e14; e15; e16]) in
      (s1,s2,s3,s4)
    end

  let test1 correlation () =
    let (s1,_,_,_) = testStreams in
    let count = ref 0 in
    let show (Ev (a, (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <%d@[%d,%d]>" !count a t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (correlation show s1))

  let test2 correlation () =
    let (s1,s2,_,_) = testStreams in
    let count = ref 0 in
    let show (Ev ((a,b), (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <(%d,%s)@[%d,%d]>" !count a b t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (correlation show s1 s2))

  let test3 correlation () =
    let (s1,s2,s3,_) = testStreams in
    let count = ref 0 in
    let show (Ev ((a,b,c), (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <(%d,%s,%.2f)@[%d,%d]>" !count a b c t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (correlation show s1 s2 s3))

  let test4 correlation () =
    let (s1,s2,s3,s4) = testStreams in
    let count = ref 0 in
    let show (Ev ((a,b,c,d), (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <(%d,%s,%.2f,%d)@[%d,%d]>" !count a b c d t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (correlation show s1 s2 s3 s4))

  let cartesian1 (type a)
        (show: a  evt -> string)
        (s1: a evt r) () =
    let module T = struct type t0 = a
                          type result = a
                   end
    in
    let module J = Join1(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      show
      (fun () ->
        S.handler
          (J.correlate (fun () ->
               J.join s1 (function
                   | ev ->
                      S.yield ev))))

  let testCartesian1 () = test1 cartesian1 ()

  let cartesian2 (type a) (type b)
        (show: (a * b) evt -> string)
        (s1: a evt r)
        (s2: b evt r) () =
    let module T = struct type t0 = a
                          type t1 = b
                          type result = a * b
                   end
    in
    let module J = Join2(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      show
      (fun () ->
        S.handler
          (J.correlate (fun () ->
               J.join (s1,s2) (function
                   | (Ev (x,i1),Ev (y,i2)) ->
                      S.yield (Ev ((x,y), i1 |@| i2))))))

  let testCartesian2 () = test2 cartesian2 ()

  let cartesian3 (type a) (type b) (type c)
        (show: (a * b * c) evt -> string)
        (s1: a evt r)
        (s2: b evt r)
        (s3: c evt r) () =
    let module T = struct type t0 = a
                          type t1 = b
                          type t2 = c
                          type result = a * b * c
                   end
    in
    let module J = Join3(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      show
      (fun () ->
        S.handler
          (J.correlate (fun () ->
               J.join (s1,s2,s3) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

  let testCartesian3 () = test3 cartesian3 ()

  let cartesian4 (type a) (type b) (type c) (type d)
        (show: (a * b * c * d) evt -> string)
        (s1: a evt r)
        (s2: b evt r)
        (s3: c evt r)
        (s4: d evt r)() =
     let module T = struct type t0 = a
                          type t1 = b
                          type t2 = c
                          type t3 = d
                          type result = a * b * c * d
                   end
    in
    let module J = Join4(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      show
      (fun () ->
        S.handler
          (J.correlate (fun () ->
               J.join (s1,s2,s3,s4) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3),Ev (w,i4)) ->
                      S.yield (Ev ((x,y,z,w), i1 |@| i2 |@| i3 |@| i4))))))

  let testCartesian4 () = test4 cartesian4 ()

  let affine3 (type a) (type b) (type c)
        (n: int)
        (i: int)
        (show: (a * b * c) evt -> string)
        (s1: a evt r)
        (s2: b evt r)
        (s3: c evt r) () =
    let module T = struct type t0 = a
                          type t1 = b
                          type t2 = c
                          type result = a * b * c
                   end
    in
    let module J = Join3(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      show
      (fun () ->
        S.handler
          (J.correlate
             ~restriction: (affine n (module J) i)
             (fun () ->
               J.join (s1,s2,s3) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

  let testAffine3_1_1 () = test3 (affine3 1 1) ()

  let aligned3 (type a) (type b) (type c)
        (show: (a * b * c) evt -> string)
        (s1: a evt r)
        (s2: b evt r)
        (s3: c evt r) () =
    let module T = struct type t0 = a
                          type t1 = b
                          type t2 = c
                          type result = a * b * c
                   end
    in
    let module J = Join3(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      show
      (fun () ->
        S.handler
          (J.correlate
             ~restriction: (align3 (module J))
             (fun () ->
               J.join (s1,s2,s3) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))


  let testAlign3 () = test3 aligned3 ()

end

module Bench = struct

    (* Stream size *)
  (* let count = 100000000 *)
  let count = 100000 (* For testing purposes *)
  let samplePeriod = count/1000 (* How often to sample *)
  let bound = 1073741823 (* 2^30 - 1, max bound that Random.int accepts *)

  let now = Unix.gettimeofday

  type stat =
    {  n_tested: int;     (* measure by pattern cont.  *)
       n_output: int;     (* that too, or by context  *)
      (* mutable t_react: float;    (\* override singleworld *\)
       * mutable t_latency: float;  (\* override context *\) *)
       throughput: float; (* derivable by count/duration in the end *)
       memory: float;     (* measure in eat *)
       duration: float }  (* measure at start/end *)

  let to_csv_row stat =
    Printf.sprintf "\"%d\",\"%d\",\"%f\",\"%f\",\"%f\""
      stat.n_tested
      stat.n_output
      (* stat.t_react
       * stat.t_latency *)
      stat.throughput
      stat.memory
      stat.duration

  let freshStat () =
   ref { n_tested = 0;
      n_output = 0;
      (* t_react = 0.0;
       * t_latency = 0.0; *)
      throughput = 0.0;
      memory = 0.0;
      duration = 0.0 }

  effect Inject: stat ref
  let inject () = perform Inject

  (* Signals end of a measurement *)
  effect Terminate: 'a
  let terminate () = perform Terminate

  module Join3Bench(T: sig type t0 type t1 type t2 type result end): (JOIN with type joined = T.t0 evt * T.t1 evt * T.t2 evt
                                                                     and type input = T.t0 evt r * T.t1 evt r * T.t2 evt r
                                                                     and type result = T.result evt)
    = struct
    module S0 = Slot(struct type t = T.t0 evt end)
    module S1 = Slot(struct type t = T.t1 evt end)
    module S2 = Slot(struct type t = T.t2 evt end)
    type joined = S0.t * S1.t * S2.t
    type result = T.result evt
    let slots: slots = [|(module S0);(module S1);(module S2)|]
    type input = S0.t r * S1.t r * S2.t r
                                     effect Join: (input * (joined -> unit)) -> unit
    let join sp f = perform (Join (sp, f))
                      effect Trigger: joined -> unit
    let trigger v = perform (Trigger v)
                      effect SetCont: (joined -> unit) -> unit
    let setCont c = perform (SetCont c)

    module Aux = struct
      let _cart f thnk1 thnk2 (x,xc) =
        flatMap
          (fun (y,yc) ->
            flatMap
              (fun (z,zc) ->
                let lives = [xc;yc;zc] in
                if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
                then
                  begin
                    List.iter (fun r -> r := Count.dec !r) lives;
                    [f (x,y,z)]
                  end
                else [])
              (thnk2 ()))
          (thnk1 ())

      let shuffle0 p = p
      let shuffle1 (x,y,z) = (y,x,z)
      let shuffle2 (x,y,z) = (y,z,x)
      let shuffle3 (x,y,z) = (y,z,x)

      (* GC the dead events all mailboxes. *)
      let cleanup () =
        Array.iter (fun (slot : (module SLOT)) ->
            let module S = (val slot) in
            S.setMail (List.filter (fun (_, c) -> Count.lt_i 0 !c) (S.getMail ()))) slots

      let lengths () = (List.length (S0.getMail ())) + (List.length (S1.getMail ())) + (List.length (S2.getMail ()))

      let eat_all (s0,s1,s2) =
        let num = ref 0 in
        let stat = inject () in
        let refreshStat () =
          begin
            match (!num mod samplePeriod) with
            | 0 -> stat := { !stat with memory = !stat.memory +. (float_of_int (lengths ())) }
            | _ -> ()
          end
        in
        let (s0,s1,s2) = (ref (Some s0), ref (Some s1), ref (Some s2)) in
        let rec select tries i = begin
            let next = (i + 1) mod 3 in
            match (tries,i) with
            |(0,_) -> print_string "yay\n";
              stat := { !stat with memory = !stat.memory /. (float_of_int (count / samplePeriod))   };
              terminate () (* all done, quit *)
            |(_,0) ->
              begin match !s0 with
              | None -> select (tries - 1) next
              | Some r -> match Async.await r with
                          | RCons (hd, tl) ->
                             begin
                               s0 := Some tl;
                               S0.push hd; (* TODO measure here *)
                               num := !num + 1;
                               refreshStat ();

                               select 3 1
                             end
                          | RNil -> s0 := None; select (tries - 1) (next)
              end
            |(_,1) ->
              begin match !s1 with
              | None -> select (tries - 1) next
              | Some r ->  match Async.await r with
                           | RCons (hd, tl) ->
                              begin
                                s1 := Some tl;
                                S1.push hd; (* TODO measure here *)
                                num := !num + 1;
                                refreshStat ();
                                select 3 2
                              end
                           | RNil -> s1 := None; select (tries - 1) (next)
              end
            |(_,2) ->
              begin match !s2 with
              | None -> select (tries - 1) next
              | Some r ->  match Async.await r with
                           | RCons (hd, tl) ->
                              begin
                                s2 := Some tl;
                                S2.push hd; (* TODO measure here *)
                                num := !num + 1;
                                refreshStat ();
                                select 3 0
                              end
                           | RNil -> s2 := None; select (tries - 1) (next)
              end
            | _ -> print_string "BAD\n"
          end
        in print_string "selecting\n"; (select 3 0)
    end
    open Aux

    (* We stage for each slot S_i a function cartesian_i,
     whic h takes an event notification x of type S_i.t and computes
     the c  ross-product of x and the contents of the mailboxes for the remaining
     slots S             _k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Com    pute the collection cartesian_i and pass its
     elements to the Complete      effect, i.e., trigger the pattern body. *)
    let cartesian0: (S0.t * Count.t ref) -> joined list =
      (_cart shuffle0 S1.getMail S2.getMail)
    let cartesian1: (S1.t * Count.t ref) -> joined list =
      (_cart shuffle1 S0.getMail S2.getMail)
    let cartesian2: (S2.t * Count.t ref) -> joined list =
      (_cart shuffle2 S0.getMail S1.getMail)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
    let assemble action =
      try action () with
      | effect (S0.Push v) k ->
         let x = List.find (fun (ev,_) -> ev = v) (S0.getMail ()) in
         forkEach trigger (cartesian0 x);
         cleanup ();
         continue k ()
      | effect (S1.Push v) k ->
         let x = List.find (fun (ev,_) -> ev = v) (S1.getMail ()) in
         forkEach trigger (cartesian1 x);
         cleanup ();
         continue k ()
      | effect (S2.Push v) k ->
         let x = List.find (fun (ev,_) -> ev = v) (S2.getMail ()) in
         forkEach trigger (cartesian2 x);
         cleanup ();
         continue k ()

    (* Handler for the ambient mailbox state *)
    let ambientState (action: unit -> unit) =
      let action =
        with_h [S0.stateHandler;
                S1.stateHandler;
                S2.stateHandler] action in
      let cont: (joined -> unit) option ref = ref None in
      try action () with
      | effect (SetCont c) k -> cont := Some c; continue k ()
      | effect (Trigger res) k ->
         match !cont with
         | Some c ->
            c res
         | None -> failwith "uninitialized join continuation"

    let correlate ?(window=(fun f -> f ())) ?(restriction=(fun f -> f ())) pattern () =
      let setup () =
        try pattern () with
        | effect (Join (streams, c)) k ->
           setCont c;
           let _ = eat_all streams in (* TODO keep the promises? *)
           ()
      in
      with_h [ambientState;
              assemble;
              restriction;
              S0.forAll;
              S1.forAll;
              S2.forAll;
              window]
        setup ()
  end

  let randStream n =
    let res = ref (Async.liftPromise RNil) in
    let rec loop i =
      if (i > 0) then begin
          res := (Async.liftPromise (RCons (Ev (Random.int bound, (i,i)), !res)));
          loop (i - 1)
        end
      else ()
    in begin
        loop n;
        !res
      end

  let measure f () =
    let _ = print_string "pre_alloc\n" in
    let a1 = randStream count in
    let a2 = randStream count in
    let a3 = randStream count in
    let stat = freshStat () in
    let start = now () in
    begin try (f a1 a2 a3 stat) with
    | effect Terminate _ -> ()
    | effect Inject k -> continue k stat
    | _ -> ()
    end;
    stat := { !stat with duration = now () -. start };
    print_string (to_csv_row !stat);
    ()
    (* stat *)

  let context stat action =
    let onDone _ =
      stat := { !stat with n_output = !stat.n_output + 1 }
    in
    ManyWorlds.handler onDone action

  let cartesian3 s1 s2 s3 stat =
    let module T = struct type t0 = int
                          type t1 = int
                          type t2 = int
                          type result = int * int * int
                   end
    in
    let module J = Join3Bench(T) in
    let module S = SingleWorld(struct type t = J.result end) in
    context
      stat
      (fun () ->
        S.handler
          (J.correlate (fun () ->
               J.join (s1,s2,s3) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                      stat := { !stat with n_tested = !stat.n_tested + 1 };
                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))


  let test () =
    Async.run (fun () -> print_string "run\n";
        (measure cartesian3))


  (*
    Join by id
    Join by timing
    Zip
    CombineLatest
    Affine
*)


end
