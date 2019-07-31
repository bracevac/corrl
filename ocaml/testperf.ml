open Prelude
open Slot
open Core
open Symantics
open Dsl
open Restriction
open Hlists
open Stat


(* TODO factor out these common parts into a module *)
module type BenchSym = sig
  include JoinExtSym
  val lift: 'a elem repr list -> 'a shape repr
end

module type BenchSymantics = (BenchSym with type 'a repr = 'a and type 'a elem = 'a evt and type 'a shape = 'a evt r)

module CB = struct
  include Cartesius
end

module Stat = struct
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
end


let rand_stream n =
  let res = ref (Async.liftPromise RNil) in
  let rec loop i =
    if (i > 0) then begin
      res := (Async.liftPromise (RCons (Ev (Random.int 1073741823, (i,i)), !res)));
      loop (i - 1)
    end
    else ()
  in begin
    loop n;
    !res
  end

let randArray n =
  Array.init n (fun i -> (Ev (Random.int 1073741823, (i,i))))


let repetitions = 10
let samples  = 10
let now = Unix.gettimeofday
(* Number of events in a generated stream *)
let event_count = 1000


(* Here is a manual expansion of what we generate *)
(* let ccons s c = CB.(from s @. (c ()))
 * let ctx0 () = CB.cnil
 * let ctx1 () = ccons (rand_stream event_count) ctx0
 * let ext1_0 () = CB.empty_ext
 * let ext1_1 () = most_recently Here
 * let ext1_2 () = affinely 1 Here
 * (\* let ext1_3 () = affinely 1 Here  TODO: integration of aligning *\)
 * let pat1: ((int * Prelude.Interval.time) * unit) -> (int * unit, int) CB.pat =
 *   fun ((x1,_), ()) -> CB.(yield x1)
 * let join1_0 () = CB.join (ctx1 ()) (ext1_0 ()) pat1
 * let join1_1 () = CB.join (ctx1 ()) (ext1_1 ()) pat1
 * let join1_2 () = CB.join (ctx1 ()) (ext1_2 ()) pat1
 *
 * let ctx2 () = ccons (rand_stream event_count) ctx1
 * let ext2_0 () = CB.empty_ext
 * let ext2_1 () = CB.((most_recently Here) |++| (most_recently (Next Here)))
 * let ext2_2 () = CB.((affinely 1 Here) |++| (affinely 1 (Next Here)))
 * (\* let ext1_3 () = affinely 1 Here  TODO: integration of aligning *\)
 * let pat2: ((int * Prelude.Interval.time) *
 *              ((int * Prelude.Interval.time) *
 *                 unit)) -> (int * (int * unit), (int * int)) CB.pat =
 *   fun ((x1,_), ((x2,_), ())) -> CB.(yield (pair x1 x2))
 * let join2_0 () = CB.join (ctx2 ()) (ext2_0 ()) pat2
 * let join2_1 () = CB.join (ctx2 ()) (ext2_1 ()) pat2
 * let join2_2 () = CB.join (ctx2 ()) (ext2_2 ()) pat2
 *
 * let ctx3 () = ccons (rand_stream event_count) ctx2
 * let ext3_0 () = CB.empty_ext
 * let ext3_1 () = CB.((most_recently Here) |++| (most_recently (Next Here)) |++| (most_recently (Next (Next Here))))
 * let ext3_2 () = CB.((affinely 1 Here) |++| (affinely 1 (Next Here)) |++| (affinely 1 (Next (Next Here))))
 * (\* let ext1_3 () = affinely 1 Here  TODO: integration of aligning *\)
 * let pat3: ((int * Prelude.Interval.time) *
 *              ((int * Prelude.Interval.time) *
 *                 ((int * Prelude.Interval.time) * unit))) -> (int * (int * (int * unit)), (int * (int * int))) CB.pat =
 *             fun ((x1,_), ((x2,_), ((x3,_), ()))) -> CB.(yield (pair x1 (pair x2 x3)))
 * let join3_0 () = CB.join (ctx3 ()) (ext3_0 ()) pat3
 * let join3_1 () = CB.join (ctx3 ()) (ext3_1 ()) pat3
 * let join3_2 () = CB.join (ctx3 ()) (ext3_2 ()) pat3 *)

module Generator = struct
  type variant = int -> string

  (* Parameters *)
  effect EventCount: int (* Size of streams *)
  let eventCount () = perform EventCount

  effect Repetitions: int
  let repetitions () = perform Repetitions

  effect Samples: int
  let samples () = perform Samples

  (* Emit code in string form somewhere *)
  effect Emit: string -> unit
  let emit: string -> unit = fun s -> perform (Emit s)
  let emits: string list -> unit = fun ss -> List.iter emit ss
  let emitln: string -> unit = fun s -> perform (Emit s); perform (Emit "\n")

  let preamble () =
    let format = Printf.sprintf in
    emitln "open Prelude";
    emitln "open Slot";
    emitln "open Core";
    emitln "open Symantics";
    emitln "open Dsl";
    emitln "open Restriction";
    emitln "open Hlists";
    emitln "open Stat";
    emitln "";
    emitln "(* Global parameters *)";
    emitln (format "let repetitions = %d" (repetitions ()));
    emitln (format "let samples = %d" (samples ()));
    emitln "let now = Unix.gettimeofday";
    (* Number of events in a generated stream *)
    emitln (format "let event_count = %d" (eventCount ()));
    emitln "let ccons s c = CB.(from s @. (c ()))";
    emitln "let ctx0 () = CB.cnil";
    emitln ""

  let rand_stream = "(rand_stream event_count)"
  let ctx i = emit (Printf.sprintf "let ctx%d () = ccons %s ctx%d\n" i rand_stream (i-1))
  let ext i j v = emit (Printf.sprintf "let ext%d_%d () = %s\n" i j v)
  let rec pat_dom = function
    | 0 -> emit "unit"
    | j when j > 0 -> emit "((int * Prelude.Interval.time) * "; (pat_dom (j - 1)); emit ")"
  let pat_cod i =
    let rec shape = function
      | 0 -> emit "unit"
      | j when j > 0 -> emit "(int * "; (shape (j - 1)); emit ")"
    in
    let rec typ = function
      | 0 -> emit "int"
      | j when j > 0 -> emit "(int * "; (typ (j - 1)); emit ")"
    in
    emit "("; (shape i); emit ", "; (typ (i - 1)); emit ") CB.pat"
  let rec pat_args = function
    | 0 -> emit "()"
    | j when j > 0 -> emit "("; emit "("; emit (Printf.sprintf "x%d" j);  emit ",_), "; (pat_args (j - 1)); emit ")"
  let rec pat_body = function
    | 1 -> emit "x1"
    | j when j > 1 -> emit "(pair "; emit (Printf.sprintf "x%d " j); (pat_body (j - 1)); emit ")"
  let pat i =
    emit (Printf.sprintf "let pat%d: " i);
    (pat_dom i); emit " -> "; (pat_cod i); emit " =\n";
    emit "  fun "; (pat_args i); emit " -> CB.(yield "; (pat_body i); emit ")\n"
  let join i j = emit (Printf.sprintf "let join%d_%d () = CB.join (ctx%d ()) (ext%d_%d ()) pat%d" i j i i j i)

  (* gen n vs generates benchmark code for all arities 1..n, where vs contains sub-generators
     of test instances, parametric over the current arity.  *)
  let gen: int -> variant list -> Buffer.t = fun n variants ->
    let format = Printf.sprintf in
    let _ = if n < 1 then failwith "Need arity >= 1" in
    let variants = (fun _ -> "CB.empty_ext") :: variants in
    let buffer = Buffer.create (16384 + 1024 * (List.length variants)) in
    let mk () =
      preamble (); emitln "(* Test instances *)";
      for i = 1 to n do
        emitln (format "(* Arity %d *)" i);
        (ctx i);
        List.iteri (fun j v -> ext i j (v j)) variants;
        pat i;
        List.iteri (fun j _ -> join i j) variants;
        emit "\n\n"
      done
    in
    match mk () with (* string are appended to an ambient buffer value via Emit  *)
    | () -> buffer
    | effect (Emit s) k -> Buffer.add_string buffer s; continue k ()
    | effect EventCount k -> continue k 1000 (*TODO move default params into a central location *)
    | effect Samples k -> continue k 10
    | effect Repetitions k -> continue k 10
end

let test_generator ?(n=3) () =
  print_string (Buffer.contents (Generator.gen n []))


(* module Join3Bench: (JOIN with type joined = int evt * int evt * int evt
 *                           and type input = int evt r * int evt r * int evt r
 *                           and type result = (int * int * int) evt)
 * = struct
 *   module S0 = Slot(struct type t = int evt end)
 *   module S1 = Slot(struct type t = int evt end)
 *   module S2 = Slot(struct type t = int evt end)
 *   type joined = S0.t * S1.t * S2.t
 *   type result = (int * int * int) evt
 *   let slots: slots = [|(module S0);(module S1);(module S2)|]
 *   type input = S0.t r * S1.t r * S2.t r
 *                  effect Join: (input * (joined -> unit)) -> unit
 *   let join sp f = perform (Join (sp, f))
 *       effect Trigger: joined -> unit
 *   let trigger v = perform (Trigger v)
 *       effect SetCont: (joined -> unit) -> unit
 *   let setCont c = perform (SetCont c)
 *
 *   module Aux = struct
 *     let _cart f thnk1 thnk2 (x,xc) =
 *       flatMap
 *         (fun (y,yc) ->
 *            flatMap
 *              (fun (z,zc) ->
 *                 let lives = [xc;yc;zc] in
 *                 if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
 *                 then
 *                   begin
 *                     List.iter (fun r -> r := Count.dec !r) lives;
 *                     [f (x,y,z)]
 *                   end
 *                 else [])
 *              (thnk2 ()))
 *         (thnk1 ())
 *
 *     let shuffle0 p = p
 *     let shuffle1 (x,y,z) = (y,x,z)
 *     let shuffle2 (x,y,z) = (y,z,x)
 *     let shuffle3 (x,y,z) = (y,z,x)
 *
 *     (\* GC the dead events all mailboxes. *\)
 *     let cleanup () =
 *       Array.iter (fun (slot : (module SLOT)) ->
 *           let module S = (val slot) in
 *           S.setMail (List.filter (fun (_, c) -> Count.lt_i 0 !c) (S.getMail ()))) slots
 *
 *     let lengths () = (List.length (S0.getMail ())) + (List.length (S1.getMail ())) + (List.length (S2.getMail ()))
 *
 *     let eat_all _ =
 *       let num = ref 0 in
 *       let (stat, (s0,s1,s2)) = inject () in
 *       let samplePeriod = arity * !stat.count / samples in
 *       let refreshStat () =
 *         begin
 *           match (!num mod samplePeriod) with
 *           | 0 -> stat := { !stat with memory = !stat.memory +. (float_of_int (lengths ())) }
 *           | _ -> ()
 *         end
 *       in
 *       let (i0,i1,i2) = (ref 0, ref 0, ref 0) in
 *       let rec select tries i =
 *         begin
 *           (\* print_int tries;
 *            * print_int i;
 *            * print_newline(); *\)
 *           let next = (i + 1) mod 3 in
 *           begin
 *             match (tries,i) with
 *             |(0,_) -> stat := { !stat with memory = !stat.memory /. (float_of_int samples)   };
 *               terminate () (\* all done, quit *\)
 *             |(_,0) ->
 *               if (!i0 < Array.length s0) then
 *                 begin
 *                   S0.push (Array.get s0 !i0);
 *                   num := !num + 1;
 *                   i0 := !i0 + 1;
 *                   refreshStat ();
 *                   select 3 1
 *                 end
 *               else
 *                 select (tries - 1) next
 *             |(_,1) ->
 *               if (!i1 < Array.length s1) then
 *                 begin
 *                   S1.push (Array.get s1 !i1);
 *                   num := !num + 1;
 *                   i1 := !i1 + 1;
 *                   refreshStat ();
 *                   select 3 2
 *                 end
 *               else
 *                 select (tries - 1) next
 *             |(_,2) ->
 *               if (!i2 < Array.length s2) then
 *                 begin
 *                   S2.push (Array.get s2 !i2);
 *                   num := !num + 1;
 *                   i2 := !i2 + 1;
 *                   refreshStat ();
 *                   select 3 0
 *                 end
 *               else
 *                 select (tries - 1) next
 *             | _ -> print_string "BAD\n"; print_newline();
 *           end
 *         end
 *       in (select 3 0)
 *   end
 *   open Aux
 *
 *   (\* We stage for each slot S_i a function cartesian_i,
 *      whic h takes an event notification x of type S_i.t and computes
 *      the c  ross-product of x and the contents of the mailboxes for the remaining
 *      slots S             _k, where k =/= i. The typical use case is interpreting the
 *      S_i.Push effect: Com    pute the collection cartesian_i and pass its
 *      elements to the Complete      effect, i.e., trigger the pattern body. *\)
 *   let cartesian0: (S0.t * Count.t ref) -> joined list =
 *     (_cart shuffle0 S1.getMail S2.getMail)
 *   let cartesian1: (S1.t * Count.t ref) -> joined list =
 *     (_cart shuffle1 S0.getMail S2.getMail)
 *   let cartesian2: (S2.t * Count.t ref) -> joined list =
 *     (_cart shuffle2 S0.getMail S1.getMail)
 *
 *   (\* The final stage in the handler stack, implementing the cartesian semantics *\)
 *   let assemble action =
 *     try action () with
 *     | effect (S0.Push v) k ->
 *       let x = List.find (fun (ev,_) -> ev = v) (S0.getMail ()) in
 *       List.iter trigger (cartesian0 x);
 *       cleanup ();
 *       continue k ()
 *     | effect (S1.Push v) k ->
 *       let x = List.find (fun (ev,_) -> ev = v) (S1.getMail ()) in
 *       List.iter trigger (cartesian1 x);
 *       cleanup ();
 *       continue k ()
 *     | effect (S2.Push v) k ->
 *       let x = List.find (fun (ev,_) -> ev = v) (S2.getMail ()) in
 *       List.iter trigger (cartesian2 x);
 *       cleanup ();
 *       continue k ()
 *
 *   (\* Handler for the ambient mailbox state *\)
 *   let ambientState (action: unit -> unit) =
 *     let action =
 *       with_h [S0.stateHandler;
 *               S1.stateHandler;
 *               S2.stateHandler] action in
 *     let cont: (joined -> unit) option ref = ref None in
 *     try action () with
 *     | effect (SetCont c) k -> cont := Some c; continue k ()
 *     | effect (Trigger res) k ->
 *       match !cont with
 *       | Some c ->
 *         continue k (c res)
 *       | None -> failwith "uninitialized join continuation"
 *
 *   let correlate ?(window=(fun f -> f ())) ?(restriction=(fun f -> f ())) pattern () =
 *     let setup () =
 *       try pattern () with
 *       | effect (Join (streams, c)) k ->
 *         setCont c;
 *         let _ = eat_all streams in (\* TODO keep the promises? *\)
 *         ()
 *     in
 *     with_h [ambientState;
 *             assemble;
 *             restriction;
 *             S0.forAll;
 *             S1.forAll;
 *             S2.forAll;
 *             window]
 *       setup ()
 * end
 *
 * let randStream n =
 *   let res = ref (Async.liftPromise RNil) in
 *   let rec loop i =
 *     if (i > 0) then begin
 *       res := (Async.liftPromise (RCons (Ev (Random.int bound, (i,i)), !res)));
 *       loop (i - 1)
 *     end
 *     else ()
 *   in begin
 *     loop n;
 *     !res
 *   end
 *
 * let randArray n =
 *   Array.init n (fun i -> (Ev (Random.int bound, (i,i))))
 *
 * let measure key count thunk =
 *   let a1 = randArray count in
 *   let a2 = randArray count in
 *   let a3 = randArray count in
 *   let stat = freshStat key count in
 *   let start = now () in
 *   begin try (thunk stat) with
 *     | effect Terminate _ -> ()
 *     | effect Inject k -> continue k (stat, (a1,a2,a3))
 *   end;
 *   let duration = now () -. start in
 *   stat := { !stat with duration = duration;
 *                        throughput = (float_of_int (arity * count)) /. duration };
 *   print_string (to_csv_row !stat);
 *   print_newline ();
 *   ()
 * (\* stat *\)
 *
 *
 * let run tests =
 *   print_string csv_header;
 *   print_newline ();
 *   List.iter
 *     (fun (key, counts, thunk) ->
 *        List.iter
 *          (fun count ->
 *             measure key count thunk)
 *          counts)
 *     tests
 *
 * let context stat action =
 *   let onDone _ =
 *     stat := { !stat with n_output = !stat.n_output + 1 }
 *   in
 *   ManyWorlds.handler onDone action
 *
 *
 * module SingleWorldBench = struct
 *   effect Yield  : Join3Bench.result -> unit
 *     effect Cancel : unit
 *
 *   let yield x = perform (Yield x)
 *   let cancel () = perform Cancel
 *
 *   let handler action =
 *     match action () with
 *     | effect (Yield v) k -> continue k ()
 *     | effect Cancel k -> continue k ()
 *     | x -> ()
 * end
 *
 * let cartesian3 stat =
 *   let dummy = toR([Ev (0, (1,1))]) in
 *   let module J = Join3Bench in
 *   let module S = SingleWorldBench in
 *   context
 *     stat
 *     (fun () ->
 *        S.handler
 *          (J.correlate (fun () ->
 *               J.join (dummy,dummy,dummy) (function
 *                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
 *                     stat := { !stat with n_tested = !stat.n_tested + 1 };
 *                     stat := { !stat with n_output = !stat.n_output + 1 };
 *                     S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))
 *
 * let mostRecent3 stat =
 *   let dummy = toR([Ev (0, (1,1))]) in
 *   let module J = Join3Bench in
 *   let module S = SingleWorldBench in
 *   context
 *     stat
 *     (fun () ->
 *        S.handler
 *          (J.correlate
 *             ~restriction: (fun action ->
 *                 (with_h [(mostRecent (module J) 0); (mostRecent (module J) 1); (mostRecent (module J) 2)]) action ())
 *             (fun () ->
 *                J.join (dummy,dummy,dummy) (function
 *                    | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
 *                      stat := { !stat with n_tested = !stat.n_tested + 1 };
 *                      stat := { !stat with n_output = !stat.n_output + 1 };
 *                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))
 *
 * let affine3_123 stat =
 *   let dummy = toR([Ev (0, (1,1))]) in
 *   let module J = Join3Bench in
 *   let module S = SingleWorldBench in
 *   context
 *     stat
 *     (fun () ->
 *        S.handler
 *          (J.correlate
 *             ~restriction: (fun action -> (with_h [(affine 1 (module J) 0); (affine 1 (module J) 1); (affine 1 (module J) 2)]) action ())
 *             (fun () ->
 *                J.join (dummy,dummy,dummy) (function
 *                    | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
 *                      stat := { !stat with n_tested = !stat.n_tested + 1 };
 *                      stat := { !stat with n_output = !stat.n_output + 1 };
 *                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))
 *
 *
 * let align3_bench (join: (module JOIN)) action =
 *   let module J = (val join) in
 *   let _ = assert ((Array.length J.slots) = 3) in
 *   let module S0 = (val (Array.get J.slots 0)) in
 *   let module S1 = (val (Array.get J.slots 1)) in
 *   let module S2 = (val (Array.get J.slots 2)) in
 *   let tryFire () = begin
 *     let m0 = List.rev (S0.getMail ()) in
 *     let m1 = List.rev (S1.getMail ()) in
 *     let m2 = List.rev (S2.getMail ()) in
 *     match (m0,m1,m2) with
 *     | ((ev0,_)::r0, (ev1,_)::r1, (ev2,_)::r2) ->
 *       let res: J.joined = Obj.magic (ev0,ev1,ev2) in (\* ugly! *\)
 *       S0.setMail (List.rev r0);
 *       S1.setMail (List.rev r1);
 *       S2.setMail (List.rev r2);
 *       J.trigger res
 *     | _ -> ()
 *   end
 *   in
 *   try action () with
 *   | effect (S0.Push _) k ->
 *     continue k (tryFire ())
 *   | effect (S1.Push _) k ->
 *     continue k (tryFire ())
 *   | effect (S2.Push _) k ->
 *     continue k (tryFire ())
 *
 * let zip3 stat =
 *   let dummy = toR([Ev (0, (1,1))]) in
 *   let module J = Join3Bench in
 *   let module S = SingleWorldBench in
 *   context
 *     stat
 *     (fun () ->
 *        S.handler
 *          (J.correlate
 *             ~restriction: (fun action ->
 *                 (with_h [(mostRecent (module J) 0); (mostRecent (module J) 1); (mostRecent (module J) 2); (align3_bench (module J))]) action ())
 *             (fun () ->
 *                J.join (dummy,dummy,dummy) (function
 *                    | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
 *                      stat := { !stat with n_tested = !stat.n_tested + 1 };
 *                      stat := { !stat with n_output = !stat.n_output + 1 };
 *                      S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))
 *
 * let tests =
 *   [
 *     ("cartesian",  [370],            cartesian3); (\* cartesian is way too slow for other input sizes  *\)
 *     ("mostRecent", [370;3700;37000;370000;3700000], mostRecent3);
 *     ("affine",     [370;3700;37000;370000;3700000], affine3_123);
 *     ("zip",        [370;3700;37000;370000;3700000], zip3)
 *   ]
 *
 * let main () = run tests
 *
 * let _ = main () *)
