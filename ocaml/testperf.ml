open Prelude
open Slot
open Main

(* Stream size *)
let arity = 3
let count = 370 (* event count for one input stream *)
let repetitions = 10
let samples  = 10
let bound = 1073741823 (* 2^30 - 1, max bound that Random.int accepts *)

let now = Unix.gettimeofday

type stat =
  {  name: string;
     count: int;
     n_tested: int;     (* measure by pattern cont.  *)
     n_output: int;     (* that too, or by context  *)
     (* mutable t_react: float;    (\* override singleworld *\)
      * mutable t_latency: float;  (\* override context *\) *)
     throughput: float; (* derivable by count/duration in the end *)
     memory: float;     (* measure in eat *)
     duration: float }  (* measure at start/end *)

let csv_header = "name,count,n_tested,n_output,throughput,memory,duration"
let to_csv_row stat =
  Printf.sprintf "\"%s\",\"%d\",\"%d\",\"%d\",\"%f\",\"%f\",\"%f\""
    stat.name
    stat.count
    stat.n_tested
    stat.n_output
    (* stat.t_react
     * stat.t_latency *)
    stat.throughput
    stat.memory
    stat.duration

let freshStat key cnt =
  ref { name = key;
        count = cnt;
        n_tested = 0;
        n_output = 0;
        (* t_react = 0.0;
         * t_latency = 0.0; *)
        throughput = 0.0;
        memory = 0.0;
        duration = 0.0 }

    effect Inject: (stat ref * (int Evt.evt array * int Evt.evt array * int Evt.evt array))
let inject () = perform Inject

    (* Signals end of a measurement *)
    effect Terminate: 'a
let terminate () = perform Terminate

module Join3Bench: (JOIN with type joined = int evt * int evt * int evt
                          and type input = int evt r * int evt r * int evt r
                          and type result = (int * int * int) evt)
= struct
  module S0 = Slot(struct type t = int evt end)
  module S1 = Slot(struct type t = int evt end)
  module S2 = Slot(struct type t = int evt end)
  type joined = S0.t * S1.t * S2.t
  type result = (int * int * int) evt
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

    let eat_all _ =
      let num = ref 0 in
      let (stat, (s0,s1,s2)) = inject () in
      let samplePeriod = arity * !stat.count / samples in
      let refreshStat () =
        begin
          match (!num mod samplePeriod) with
          | 0 -> stat := { !stat with memory = !stat.memory +. (float_of_int (lengths ())) }
          | _ -> ()
        end
      in
      let (i0,i1,i2) = (ref 0, ref 0, ref 0) in
      let rec select tries i =
        begin
          (* print_int tries;
           * print_int i;
           * print_newline(); *)
          let next = (i + 1) mod 3 in
          begin
            match (tries,i) with
            |(0,_) -> stat := { !stat with memory = !stat.memory /. (float_of_int samples)   };
              terminate () (* all done, quit *)
            |(_,0) ->
              if (!i0 < Array.length s0) then
                begin
                  S0.push (Array.get s0 !i0);
                  num := !num + 1;
                  i0 := !i0 + 1;
                  refreshStat ();
                  select 3 1
                end
              else
                select (tries - 1) next
            |(_,1) ->
              if (!i1 < Array.length s1) then
                begin
                  S1.push (Array.get s1 !i1);
                  num := !num + 1;
                  i1 := !i1 + 1;
                  refreshStat ();
                  select 3 2
                end
              else
                select (tries - 1) next
            |(_,2) ->
              if (!i2 < Array.length s2) then
                begin
                  S2.push (Array.get s2 !i2);
                  num := !num + 1;
                  i2 := !i2 + 1;
                  refreshStat ();
                  select 3 0
                end
              else
                select (tries - 1) next
            | _ -> print_string "BAD\n"; print_newline();
          end
        end
      in (select 3 0)
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
      List.iter trigger (cartesian0 x);
      cleanup ();
      continue k ()
    | effect (S1.Push v) k ->
      let x = List.find (fun (ev,_) -> ev = v) (S1.getMail ()) in
      List.iter trigger (cartesian1 x);
      cleanup ();
      continue k ()
    | effect (S2.Push v) k ->
      let x = List.find (fun (ev,_) -> ev = v) (S2.getMail ()) in
      List.iter trigger (cartesian2 x);
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
        continue k (c res)
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

let randArray n =
  Array.init n (fun i -> (Ev (Random.int bound, (i,i))))

let measure key count thunk =
  let a1 = randArray count in
  let a2 = randArray count in
  let a3 = randArray count in
  let stat = freshStat key count in
  let start = now () in
  begin try (thunk stat) with
    | effect Terminate _ -> ()
    | effect Inject k -> continue k (stat, (a1,a2,a3))
  end;
  let duration = now () -. start in
  stat := { !stat with duration = duration;
                       throughput = (float_of_int (arity * count)) /. duration };
  print_string (to_csv_row !stat);
  print_newline ();
  ()
(* stat *)


let run tests =
  print_string csv_header;
  print_newline ();
  List.iter
    (fun (key, counts, thunk) ->
       List.iter
         (fun count ->
            measure key count thunk)
         counts)
    tests

let context stat action =
  let onDone _ =
    stat := { !stat with n_output = !stat.n_output + 1 }
  in
  ManyWorlds.handler onDone action


module SingleWorldBench = struct
  effect Yield  : Join3Bench.result -> unit
    effect Cancel : unit

  let yield x = perform (Yield x)
  let cancel () = perform Cancel

  let handler action =
    match action () with
    | effect (Yield v) k -> continue k ()
    | effect Cancel k -> continue k ()
    | x -> ()
end

let cartesian3 stat =
  let dummy = toR([Ev (0, (1,1))]) in
  let module J = Join3Bench in
  let module S = SingleWorldBench in
  context
    stat
    (fun () ->
       S.handler
         (J.correlate (fun () ->
              J.join (dummy,dummy,dummy) (function
                  | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                    stat := { !stat with n_tested = !stat.n_tested + 1 };
                    stat := { !stat with n_output = !stat.n_output + 1 };
                    S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

let mostRecent3 stat =
  let dummy = toR([Ev (0, (1,1))]) in
  let module J = Join3Bench in
  let module S = SingleWorldBench in
  context
    stat
    (fun () ->
       S.handler
         (J.correlate
            ~restriction: (fun action ->
                (with_h [(mostRecent (module J) 0); (mostRecent (module J) 1); (mostRecent (module J) 2)]) action ())
            (fun () ->
               J.join (dummy,dummy,dummy) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                     stat := { !stat with n_tested = !stat.n_tested + 1 };
                     stat := { !stat with n_output = !stat.n_output + 1 };
                     S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

let affine3_123 stat =
  let dummy = toR([Ev (0, (1,1))]) in
  let module J = Join3Bench in
  let module S = SingleWorldBench in
  context
    stat
    (fun () ->
       S.handler
         (J.correlate
            ~restriction: (fun action -> (with_h [(affine 1 (module J) 0); (affine 1 (module J) 1); (affine 1 (module J) 2)]) action ())
            (fun () ->
               J.join (dummy,dummy,dummy) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                     stat := { !stat with n_tested = !stat.n_tested + 1 };
                     stat := { !stat with n_output = !stat.n_output + 1 };
                     S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))


let align3_bench (join: (module JOIN)) action =
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
      J.trigger res
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

let zip3 stat =
  let dummy = toR([Ev (0, (1,1))]) in
  let module J = Join3Bench in
  let module S = SingleWorldBench in
  context
    stat
    (fun () ->
       S.handler
         (J.correlate
            ~restriction: (fun action ->
                (with_h [(mostRecent (module J) 0); (mostRecent (module J) 1); (mostRecent (module J) 2); (align3_bench (module J))]) action ())
            (fun () ->
               J.join (dummy,dummy,dummy) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                     stat := { !stat with n_tested = !stat.n_tested + 1 };
                     stat := { !stat with n_output = !stat.n_output + 1 };
                     S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

let tests =
  [
    ("cartesian",  [370],            cartesian3); (* cartesian is way too slow for other input sizes  *)
    ("mostRecent", [370;3700;37000;370000;3700000], mostRecent3);
    ("affine",     [370;3700;37000;370000;3700000], affine3_123);
    ("zip",        [370;3700;37000;370000;3700000], zip3)
  ]

let main () = run tests

let _ = main ()
