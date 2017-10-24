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

(* A slot x represents a binding 'x from ...' inside of a correlate block.
   Each binding has specific effects attached to it (generative effects).
   *)
module type SLOT = sig
  (* The type of event values this slot binds *)
  type t
  (* Push event notification *)
  effect Push: t -> unit
  val push: t -> unit
  (* Retrieve mailbox state, mail is ref-counted *)
  effect GetMail: (t * int) list
  val getMail: unit -> (t * int) list
  (* Set mailbox state *)
  effect SetMail: (t * int) list -> unit
  val setMail: (t * int) list -> unit
  val stateHandler: (unit -> 'a) -> 'a
  val forAll: (unit -> 'a) -> 'a
end

module Slot(T: SomeT): (SLOT with type t = T.t) = struct
  type t = T.t
  effect Push: t -> unit
  let push v = perform (Push v)
  effect GetMail: (t * int) list
  let getMail () = perform GetMail
  effect SetMail: (t * int) list -> unit
  let setMail l = perform (SetMail l)

  let stateHandler action =
    let mbox: (t * int) list ref = ref [] in (* TODO avoid mutability *)
    try action () with
    | effect GetMail k -> continue k !mbox
    | effect (SetMail l) k -> mbox := l; continue k ()

  let forAll action =
    try action () with
    | effect (Push x) k ->
       setMail ((x,0) :: (getMail ()));
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

let inc_snd n l = List.map (fun (y,c) -> (y,c+n)) l

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
    let _cart f thnk1 thnk2 thnk3 x =
      flatMap
        (fun y ->
          flatMap
            (fun z ->
              List.map (fun w -> f (x,y,z,w))
                (thnk3 ()))
            (thnk2 ()))
        (thnk1 ())

    let shuffle0 p = p
    let shuffle1 (x,y,z,w) = (y,x,z,w)
    let shuffle2 (x,y,z,w) = (y,z,x,w)
    let shuffle3 (x,y,z,w) = (y,z,w,x)

    (* mailbox_i, without the refcounts *)
    let mail0 () = List.map fst (S0.getMail())
    let mail1 () = List.map fst (S1.getMail())
    let mail2 () = List.map fst (S2.getMail())
    let mail3 () = List.map fst (S3.getMail())

    (* For a S_i.Push v effect, computes the refcount of v, which is the product over |mailbox_k|, k =/= i*)
    let refcounts0 () =
      (List.length (S1.getMail ())) * (List.length (S2.getMail())) * (List.length (S3.getMail ()))
    let refcounts1 () =
      (List.length (S0.getMail ())) * (List.length (S2.getMail())) * (List.length (S3.getMail ()))
    let refcounts2 () =
      (List.length (S0.getMail ())) * (List.length (S1.getMail())) * (List.length (S3.getMail ()))
    let refcounts3 () =
      (List.length (S0.getMail ())) * (List.length (S1.getMail())) * (List.length (S2.getMail ()))
    (* The effect of a S_i.Push x on each mailbox_k *)
    let inc_refcounts0 x =
      S1.setMail ((inc_snd 1) (S1.getMail()));
      S2.setMail ((inc_snd 1) (S2.getMail()));
      S3.setMail ((inc_snd 1) (S3.getMail()));
      S0.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts0 ())))
                    (S0.getMail ()))
    let inc_refcounts1 x =
      S0.setMail ((inc_snd 1) (S0.getMail()));
      S2.setMail ((inc_snd 1) (S2.getMail()));
      S3.setMail ((inc_snd 1) (S3.getMail()));
      S1.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts1 ())))
                    (S1.getMail ()))
    let inc_refcounts2 x =
      S0.setMail ((inc_snd 1) (S0.getMail()));
      S1.setMail ((inc_snd 1) (S1.getMail()));
      S3.setMail ((inc_snd 1) (S3.getMail()));
      S2.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts2 ())))
                    (S2.getMail ()))
    let inc_refcounts3 x =
      S0.setMail ((inc_snd 1) (S0.getMail()));
      S1.setMail ((inc_snd 1) (S1.getMail()));
      S2.setMail ((inc_snd 1) (S2.getMail()));
      S3.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts3 ())))
                    (S3.getMail ()))

    let eat_all (s0,s1,s2,s3) =
      let thunks = [(fun () -> Reactive.eat S0.push s0);
                    (fun () -> Reactive.eat S1.push s1);
                    (fun () -> Reactive.eat S2.push s2);
                    (fun () -> Reactive.eat S3.push s3)] in
      List.iter (fun f -> f ()) thunks
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: S0.t -> joined list =
    (_cart shuffle0 mail1 mail2 mail3)
  let cartesian1: S1.t -> joined list =
    (_cart shuffle1 mail0 mail2 mail3)
  let cartesian2: S2.t -> joined list =
    (_cart shuffle2 mail0 mail1 mail3)
  let cartesian3: S3.t -> joined list =
    (_cart shuffle3 mail0 mail1 mail2)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       inc_refcounts0 v;
       forkEach trigger (cartesian0 v);
       continue k ()
    | effect (S1.Push v) k ->
       inc_refcounts1 v;
       forkEach trigger (cartesian1 v);
       continue k ()
    | effect (S2.Push v) k ->
       inc_refcounts2 v;
       forkEach trigger (cartesian2 v);
       continue k ()
    | effect (S3.Push v) k ->
       inc_refcounts3 v;
       forkEach trigger (cartesian3 v);
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
  let join sp f = perform (Join (sp,f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    let _cart f thnk1 thnk2 x =
      flatMap
        (fun y ->
          List.map (fun z -> f (x,y,z))
            (thnk2 ()))
        (thnk1 ())


    let shuffle0 p = p
    let shuffle1 (x,y,z) = (y,x,z)
    let shuffle2 (x,y,z) = (y,z,x)

    (* mailbox_i, without the refcounts *)
    let mail0 () = List.map fst (S0.getMail())
    let mail1 () = List.map fst (S1.getMail())
    let mail2 () = List.map fst (S2.getMail())

    (* For a S_i.Push v effect, computes the refcount of v, which is the product over |mailbox_k|, k =/= i*)
    let refcounts0 () =
      (List.length (S1.getMail ())) * (List.length (S2.getMail()))
    let refcounts1 () =
      (List.length (S0.getMail ())) * (List.length (S2.getMail()))
    let refcounts2 () =
      (List.length (S0.getMail ())) * (List.length (S1.getMail()))
    (* The effect of a S_i.Push x on each mailbox_k *)
    let inc_refcounts0 x =
      S1.setMail ((inc_snd 1) (S1.getMail()));
      S2.setMail ((inc_snd 1) (S2.getMail()));
      S0.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts0 ())))
                    (S0.getMail ()))
    let inc_refcounts1 x =
      S0.setMail ((inc_snd 1) (S0.getMail()));
      S2.setMail ((inc_snd 1) (S2.getMail()));
      S1.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts1 ())))
                    (S1.getMail ()))
    let inc_refcounts2 x =
      S0.setMail ((inc_snd 1) (S0.getMail()));
      S1.setMail ((inc_snd 1) (S1.getMail()));
      S2.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts2 ())))
                    (S2.getMail ()))

    let eat_all (s0,s1,s2) =
      let thunks = [(fun () -> Reactive.eat S0.push s0);
                    (fun () -> Reactive.eat S1.push s1);
                    (fun () -> Reactive.eat S2.push s2)] in
      List.iter (fun f -> f ()) thunks
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: S0.t -> joined list =
    (_cart shuffle0 mail1 mail2)
  let cartesian1: S1.t -> joined list =
    (_cart shuffle1 mail0 mail2)
  let cartesian2: S2.t -> joined list =
    (_cart shuffle2 mail0 mail1)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       inc_refcounts0 v;
       forkEach trigger (cartesian0 v);
       continue k ()
    | effect (S1.Push v) k ->
       inc_refcounts1 v;
       forkEach trigger (cartesian1 v);
       continue k ()
    | effect (S2.Push v) k ->
       inc_refcounts2 v;
       forkEach trigger (cartesian2 v);
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState action =
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
      | effect (Join (streams,c)) k ->
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
  let slots: slots = [|(module S0);(module S1)|]
  type input = S0.t r * S1.t r
  type result = T.result evt
  effect Join: (input * (joined -> unit)) -> unit
  let join sp f = perform (Join (sp,f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    let _cart f thnk1 x =
      List.map
        (fun y -> f (x,y))
        (thnk1 ())

    let shuffle0 p = p
    let shuffle1 (x,y) = (y,x)

    (* mailbox_i, without the refcounts *)
    let mail0 () = List.map fst (S0.getMail())
    let mail1 () = List.map fst (S1.getMail())

    (* For a S_i.Push v effect, computes the refcount of v, which is the product over |mailbox_k|, k =/= i*)
    let refcounts0 () =
      (List.length (S1.getMail ()))
    let refcounts1 () =
      (List.length (S0.getMail ()))
    (* The effect of a S_i.Push x on each mailbox_k *)
    let inc_refcounts0 x =
      S1.setMail ((inc_snd 1) (S1.getMail()));
      S0.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts0 ())))
                    (S0.getMail ()))
    let inc_refcounts1 x =
      S0.setMail ((inc_snd 1) (S0.getMail()));
      S1.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts1 ())))
                    (S1.getMail ()))
    let eat_all (s0,s1) =
      let thunks = [(fun () -> Reactive.eat S0.push s0);
                    (fun () -> Reactive.eat S1.push s1)] in
      (* let res = Async.interleaved thunks in *) (*TODO make interleaved *)
      List.iter (fun f -> f ()) thunks;
  end
  open Aux

  (* We stage for each slot S_i a function cartesian_i,
     which takes an event notification x of type S_i.t and computes
     the cross-product of x and the contents of the mailboxes for the remaining
     slots S_k, where k =/= i. The typical use case is interpreting the
     S_i.Push effect: Compute the collection cartesian_i and pass its
     elements to the Complete effect, i.e., trigger the pattern body. *)
  let cartesian0: S0.t -> joined list =
    (_cart shuffle0 mail1)
  let cartesian1: S1.t -> joined list =
    (_cart shuffle1 mail0)

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       inc_refcounts0 v;
       forkEach trigger (cartesian0 v);
       continue k ()
    | effect (S1.Push v) k ->
       inc_refcounts1 v;
       forkEach trigger (cartesian1 v);
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState action =
    let action =
      with_h [S0.stateHandler;
              S1.stateHandler] action in
    let cont: (joined -> unit) option ref = ref None in (*TODO avoid mutability*)
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
      | effect (Join (streams, callback)) k ->
           setCont callback;
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
  let slots: slots = [|(module S0)|]
  type input = S0.t r
  type result = T.result evt
  effect Join: (input * (joined -> unit)) -> unit
  let join sp f = perform (Join (sp,f))
  effect Trigger: joined -> unit
  let trigger v = perform (Trigger v)
  effect SetCont: (joined -> unit) -> unit
  let setCont c = perform (SetCont c)

  module Aux = struct
    (* For a S_i.Push v effect, computes the refcount of v, which is the product over |mailbox_k|, k =/= i*)
    let refcounts0 () = 1
    (* The effect of a S_i.Push x on each mailbox_k *)
    let inc_refcounts0 x =
      S0.setMail (update_first
                    (fun (y,_) -> y == x)
                    (fun (x,c) -> (x,c+(refcounts0 ())))
                    (S0.getMail ()))
    let eat_all s0 =
      let thunks = [(fun () -> Reactive.eat S0.push s0)] in
      List.iter (fun f -> f ()) thunks
  end
  open Aux

  let cartesian0: S0.t -> joined list = (fun x -> [x])

  (* The final stage in the handler stack, implementing the cartesian semantics *)
  let assemble action =
    try action () with
    | effect (S0.Push v) k ->
       inc_refcounts0 v;
       forkEach trigger (cartesian0 v);
       continue k ()

  (* Handler for the ambient mailbox state *)
  let ambientState action =
    let action = (fun () -> S0.stateHandler action) in
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
      | effect (Join (streams, f)) k ->
         setCont f;
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
  let module S = (val (Array.get J.slots i)) in
  try action () with
  | effect (S.Push v) k ->
     S.setMail([(v,0)]);
     continue k (S.push v)

(** Restrict the ith slot of a join such that each event for i is associated at most once in a product *)
let affine (join: (module JOIN)) i action =
  let module J = (val join) in
  let module Si = (val (Array.get J.slots i)) in
  let filter_i () = Si.setMail (List.filter (fun (_,c) -> c == 0) (Si.getMail ())) in
  (* We intercept any push effect that a join may process (= potential change of S_i inbox)
     and simply clear out values from S_i's inbox *)
  let wrap (slot: (module SLOT)) thunk =
    begin
      let module S = (val slot) in
      let thunk () =
        try thunk () with
        | effect (S.Push v) k ->
           S.push v;    (* First let upstream apply the effect. *)
           filter_i (); (* Check what needs to be GC'd. *)
           continue k ()
      in thunk
    end
  in Array.fold_right wrap J.slots action

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

  let testCartesian1 () =
    let (s1,_,_,_) = testStreams in
    let count = ref 0 in
    let show (Ev (a, (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <%d@[%d,%d]>" !count a t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (cartesian1 show s1))


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

  let testCartesian2 () =
    let (s1,s2,_,_) = testStreams in
    let count = ref 0 in
    let show (Ev ((a,b), (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <(%d,%s)@[%d,%d]>" !count a b t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (cartesian2 show s1 s2))

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

  let testCartesian3 () =
    let (s1,s2,s3,_) = testStreams in
    let count = ref 0 in
    let show (Ev ((a,b,c), (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <(%d,%s,%.2f)@[%d,%d]>" !count a b c t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (cartesian3 show s1 s2 s3))

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

  let testCartesian4 () =
    let (s1,s2,s3,s4) = testStreams in
    let count = ref 0 in
    let show (Ev ((a,b,c,d), (t1,t2))) =
      count := !count + 1;
      Printf.sprintf "%d. <(%d,%s,%.2f,%d)@[%d,%d]>" !count a b c d t1 t2
    in
    Async.run (fun () ->
        DelimCont.reset (cartesian4 show s1 s2 s3 s4))

end
