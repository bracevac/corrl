(** Benchmarks statistics *)


type t =
  {  name: string;
     arity: int;        (* Arity *)
     count: int;        (* Total number of events, will be divided evenly *)
     sample_freq: int;  (* Sample frequency in number of events observed *)
     mutable cursor: int; (* how many of the #count events have been pushed so far?*)
     mutable n_tested: int64;     (* Number of tuples tested against pattern  *)
     mutable n_output: int64;     (* Number of tuples yielded *)
     t_latency: Mtime.span Queue.t;
     mutable aux_c_latency: Mtime_clock.counter option; (* current latency time counter *)
     t_throughput: float Queue.t; (* samples of throughput, in #events/s *)
     mutable aux_c_throughput: (int * int64 * Mtime_clock.counter) option; (* throughput cursor, n_output at  and time counter when sample started  *)
     aux_throughput_duration: int; (* How many input events to wait until finishing the throughput sample *)
     t_gc: Mtime.span Queue.t;       (* avg. time spent garbage collecting mail in reify (ns) *)
     memory: int Queue.t;     (* measure in eat *)
     mutable t_duration: Mtime.span }  (* measure at start/end (seconds) *)

let fresh_stat name arity event_count freq =
      { name = name;
        arity = arity;
        count = event_count;
        sample_freq = freq;
        cursor = 0;
        n_tested = 0L;
        n_output = 0L;
        t_latency = Queue.create ();
        aux_c_latency = None;
        t_throughput = Queue.create ();
        aux_c_throughput = None;
        aux_throughput_duration = (int_of_float (0.9 *. (float_of_int freq)));
        t_gc = Queue.create ();
        memory = Queue.create ();
        t_duration = Mtime.Span.zero }

let finalize stat counter =
  stat.t_duration <- Mtime_clock.count counter

let gc_time stat action =
  if ((stat.cursor mod stat.sample_freq) = 0) then
    let start = Mtime_clock.now () in
    begin
      action ();
      Queue.push (Mtime.span (Mtime_clock.now ()) start) stat.t_gc
    end
  else ()

let mem_sample stat thnk =
  if ((stat.cursor mod stat.sample_freq) = 0) then
    Queue.push (thnk ()) stat.memory
  else ()

let begin_latency_sample stat  =
  match stat.aux_c_latency, (stat.cursor mod stat.sample_freq) with
  | None, 0 ->
     stat.aux_c_latency <- Some (Mtime_clock.counter ())
  | _, _ -> ()

let end_latency_sample stat =
  match stat.aux_c_latency with
  | Some c ->
     Queue.push (Mtime_clock.count c) stat.t_latency;
     stat.aux_c_latency <- None
  | _ -> ()

let begin_throughput_sample stat  =
  match stat.aux_c_throughput, (stat.cursor mod stat.sample_freq) with
  | None, 0 ->
     stat.aux_c_throughput <- Some (stat.cursor, stat.n_output, (Mtime_clock.counter ()))
  | _, _ -> ()

let end_throughput_sample stat =
  match stat.aux_c_throughput with
  | Some (cursor, n_out, c) when (stat.cursor - cursor) >= stat.aux_throughput_duration ->
     let time = Mtime.Span.to_s (Mtime_clock.count c) in
     let delta = Int64.(to_float (sub stat.n_output n_out)) in
     Queue.push (delta /. time) stat.t_throughput;
     stat.aux_c_throughput <- None
  | _ -> ()



effect InjectStat: t
let injectStat () = perform InjectStat
effect Terminate: 'a
let terminate () = perform Terminate

let format_f = Printf.sprintf "%f"

let summary_row stat =
  [stat.name;  string_of_int stat.arity;  string_of_int stat.count;
   Int64.to_string stat.n_tested; Int64.to_string stat.n_output; format_f (Mtime.Span.to_s stat.t_duration)]

let t_latency_col stat =
  List.(map (fun x -> format_f (Mtime.Span.to_ns x)) (Prelude.list_of_queue stat.t_latency))

let t_throughput_col stat =
  List.(map (format_f) (Prelude.list_of_queue stat.t_throughput))

let t_gc_col stat =
  List.(map (fun x -> format_f (Mtime.Span.to_ns x)) (Prelude.list_of_queue stat.t_gc))

let memory_col stat =
  List.(map (string_of_int) (Prelude.list_of_queue stat.memory))

let csv_header = "name,arity,count,n_tested,n_output,t_duration_s,t_latency_ns,t_throughput_ev_s,t_gc_ns,memory"

let table stat =
  let cols = [t_latency_col stat; t_throughput_col stat; t_gc_col stat; memory_col stat] in
  let rowcount = List.(fold_right (max) (map (length) cols) 0) in
  let cols = List.map (fun col ->
                 let padding = List.(init (rowcount - (length col)) (fun _ -> "")) in
                 col @ padding) cols in
  let rows_left =
    let summary_row = summary_row stat in
    let n = List.length summary_row in
    let padding_rows = List.(init (rowcount - 1) (fun _ -> init n (fun _ -> ""))) in
    summary_row :: padding_rows
  in
  let rows_right =
    let catpose c = function
      | [] -> List.map (fun x -> [x]) c
      | rows -> List.fold_right2 (fun a b r ->  (a :: b) :: r) c rows []
    in
    List.(fold_right catpose cols [])
  in
  List.(map (fun (r1,r2) -> r1 @ r2) (combine rows_left rows_right))

let to_csv ?(header=csv_header) stat =
  let t = table stat in
  String.(concat "\n" (header :: (List.map (String.concat ",") t))) ^ "\n"
