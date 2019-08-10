
(* Retargetable benchmark code generator *)
module Gen = struct
  (* Represents the code of a join instance (restriction handler to be
     injected into a join).  Given the current arity to instantiate,
     it will emit code of the declaration for the restriction handler
     definition *)
  type restriction = {
      name: string;
      code: int -> unit -> unit
  }

  (* Emit code in string form somewhere *)
  effect Emit: string -> unit
  let emit: string -> unit = fun s -> perform (Emit s)
  let emits: string list -> unit = fun ss -> List.iter emit ss
  let emitln: string -> unit = fun s -> perform (Emit s); perform (Emit "\n")

  let separator sep stop n i =
    if (i + 1) = n then emit stop
    else emit sep

  let extplus = separator " |++| " ""
  let extat = separator " @@ " " mz"
  let ctxat = separator " @. " " @. cnil"

  let enclose ?(left="(") ?(right=")") f = emit left; f(); emit right
  let enclose' ?(left="(") ?(right=")") s = emit left; emit s; emit right

  let indent ?(n=2) action =
    try action () with
    | effect (Emit s) k -> emit (String.make n ' '); continue k (emit s)

  let range name sep f n () =
        for i = 0 to (n - 1) do
          enclose (fun () -> emit name; emit " "; (f i)); (sep n i)
        done

  let preamble () =
    emitln "open Prelude";
    emitln "open Slot";
    emitln "open Core2";
    emitln "open Symantics";
    emitln "open Dsl2";
    emitln "open Restriction2";
    emitln "open Hlists";
    emitln "open HPointers";
    emitln "open Bench_common";
    emitln "open Stat";
    emitln "";
    emitln "let instances = Queue.create ()";
    emitln ""

  let rand_stream = "from (rand_stream event_count)"
  let ctx' i = emit "CB."; enclose (fun () -> (range rand_stream ctxat (fun _ -> ()) i ()))
  let ctx i = emit (Printf.sprintf "let ctx%d () = " i); ctx' i; emit "\n"

  let ext i j {name = s; code = v} =
    emit (Printf.sprintf "(* %s%d_%d *)\n" s i j);
    emit (Printf.sprintf "let ext%d_%d () = " i j); v i (); emitln ""

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
  let join i j =
    emit (Printf.sprintf "let join%d_%d () = CB.join (ctx%d ()) (ext%d_%d ()) pat%d\n" i j i i j i)
  let add_instance n i {name = s; code = _} = emit (Printf.sprintf "let _ = Queue.add (\"%s\", %d, join%d_%d) instances\n" s n n i)

  (* one_code n vs () generates the test instance code for one given arity n and list of variants vs.
     An instance has the form:
     let ctx{n} () = <bind n random streams>
     (let ext{n}_{i} () = <variant i>)_{1 <= i <= k} where vs = variant_1 ... variant_k
     let pat{n} = <n-ary join pattern abstration>
     (let join{n}_{i} = join ctx{n} ext{n}_{i} pat{n})_{1 <= i <= k}
   *)
  let one_code: int -> restriction list -> unit -> unit = fun i variants () ->
    let format = Printf.sprintf in
    let _ = if i < 1 then failwith "Need arity >= 1" in
    emitln (format "(* Arity %d *)" i);
    (ctx i);
    List.iteri (fun j v -> ext i j v) variants;
    pat i;
    List.iteri (fun j v -> join i j) variants;
    List.iteri (fun j v ->  add_instance i j v) variants

  (* all_codes n vs () generates the test instances for all
     arities from 1 to n (cf. one_code above).
     *)
  let all_codes: int -> restriction list -> unit -> unit = fun n variants () ->
    for i = 1 to n do
      one_code i variants ();
      emit "\n\n"
    done

  let (|>|) a1 a2 = fun () -> a1 (); a2 ()

  let add_preamble () =
    preamble (); emitln "(* Test instances *)"

  let add_run title () =
    emit "let _ = measure \""; emit title; emitln "\" instances"

  let rec digits =
    function
    | i when i >= 0 && i <= 9 -> 1
    | i when i > 9 -> 1 + (digits (i / 10))

  let title n = function
    | i when i > 0 && i <= n -> Printf.sprintf "perf%0*d" (digits n) i

  let filename n i = (title n i) ^ ".ml"

  let to_file name action =
    let oc = open_out name in
    match action () with
    | x -> close_out oc; x
    | effect (Emit s) k -> continue k (output_string oc s)

  let to_buffer action =
    let buffer = Buffer.create 16384 in
    match action () with
    | x -> buffer
    | effect (Emit s) k -> continue k (Buffer.add_string buffer s)

  let gen_one: int -> restriction list -> string -> unit -> unit = fun n variants title ->
    add_preamble |>| (one_code n variants)  |>| (add_run title)

  let gen_all: int -> restriction list -> string -> unit -> unit = fun n variants title ->
    add_preamble |>| (all_codes n variants) |>| (add_run title)

  let separate_files: int -> restriction list -> unit = fun n variants ->
    for i = 1 to n do
      to_file (filename n i) (gen_one i variants (title n i))
    done

  let single_file: int -> restriction list -> unit = fun n variants ->
    to_file (filename n n) (gen_all n variants (title n n))

  let in_buffer: int -> restriction list -> Buffer.t = fun n variants ->
    to_buffer (gen_all n variants (title n n))
end

module Extensions = struct
  open Gen

  let rec ptr = function
    | 0 -> emit "pz"
    | i when i > 0 -> emit "(ps "; (ptr (i - 1)); emit ")"

  let mptrs n () =
    range "ms" extat ptr n ()

  let most_recently n () = emit "CB."; enclose (fun () -> range "most_recently" extplus ptr n ())
  let affinely n () = emit "CB."; enclose (fun () -> range "affinely 1" extplus ptr n ())
  let aligning n () =
    emit "CB."; enclose (fun () ->
                    enclose (fun () -> emit "aligning "; enclose (fun () -> mptrs n ()));
                    emit " |++| "; (range "most_recently" extplus ptr n ()))

  let list = [{name = "cartesian";     code = (fun _ _ -> emit "CB.empty_ext") };
              {name = "most_recently"; code = most_recently};
              {name = "affinely";      code = affinely};
              {name = "aligning";      code = aligning}]
end

let print_code ?(n=3) ?(xts=Extensions.list) () =
  print_string (Buffer.contents (Gen.in_buffer n xts))

let write_code ?(n=3) ?(xts=Extensions.list) () =
  Gen.separate_files n xts

let write_dune_file n =
  let oc = open_out "dune.benchmark" in
  let out = output_string oc in
  out "(executables \n";
    out "  (names ";
    for i = 1 to n do
      out (Printf.sprintf "%s " (Gen.title n i))
    done;
    out ")\n";
  out "(libraries unix oml)";
  out ")\n";
  close_out oc

let arity = ref None
let set_arity n =
  arity := Some n

let _ =
  let speclist = [("-n", Arg.Int (set_arity), "Sets the arity (mandatory)")] in
  let usage_msg = "Generate benchmarks"
  in Arg.parse speclist print_endline usage_msg;
     match !arity with
     | Some n when n > 0 ->
        write_code ~n:n ();
        write_dune_file n
     | _ -> print_string (Arg.usage_string speclist usage_msg)


(*
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
 *
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
 * let tests =
 *   [
 *     ("cartesian",  [370],            cartesian3); (\* cartesian is way too slow for other input sizes  *\)
 *     ("mostRecent", [370;3700;37000;370000;3700000], mostRecent3);
 *     ("affine",     [370;3700;37000;370000;3700000], affine3_123);
 *     ("zip",        [370;3700;37000;370000;3700000], zip3)
 *   ]
*)
