open Prelude
open Slot
open Suspension

module type SINGLEWORLD = sig
  type t
  effect Yield  : t -> unit
  effect Cancel : unit

  val yield: t -> unit
  val cancel: unit -> unit

  val handler: (unit -> 'a) -> t option
end

(* TODO we can get rid of the nondeterminism effect *)
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


let rec forkEach f = function
  | [] -> ()
  | x::xs -> if (ManyWorlds.fork ()) then (f x) else (forkEach f xs)

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
