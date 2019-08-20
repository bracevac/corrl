(** Benchmarks statistics *)

type t =
  {  name: string;
     arity: int;        (* Arity *)
     count: int;        (* Number of events per stream *)
     sample_freq: int;  (* Sample frequency in number of events observed *)
     mutable n_tested: int64;     (* Number of tuples tested against pattern  *)
     mutable n_output: int64;     (* Number of tuples yielded *)
     mutable t_latency: float;  (* avg. latency (ns) *)
     mutable aux_n_latency: int; (* number of latency measurements  *)
     mutable aux_t_latency: Mtime.span; (* accumulated latency measurements *)
     mutable aux_c_latency: Mtime_clock.counter option; (* current latency time counter *)
     mutable t_gc: float;       (* avg. time spent garbage collecting mail in reify (ns) *)
     mutable aux_t_gc: Mtime.span; (* accumulated gc times *)
     mutable aux_n_gc: int;     (* number of gc time measurements *)
     mutable throughput: float; (* derivable by count/duration in the end *)
     mutable memory: float;     (* measure in eat *)
     mutable n_memory_samples: int;     (* measure in eat *)
     mutable t_duration: Mtime.span }  (* measure at start/end (seconds) *)

(* type t' =
 *   {  name: string;
 *      arity: int;        (\* Arity *\)
 *      count: int;        (\* Number of events per stream *\)
 *      sample_freq: int;  (\* Sample frequency in number of events observed *\)
 *      mutable n_tested: int64;     (\* Number of tuples tested against pattern  *\)
 *      mutable n_output: int64;     (\* Number of tuples yielded *\)
 *      mutable t_latency: Mtime.span array;
 *      mutable aux_c_latency: Mtime_clock.counter option; (\* current latency time counter *\)
 *      mutable t_throughput: (int * Mtime.span) array;
 *      mutable aux_c_throughput: Mtime_clock.counter option; (\* current throughput time counter *\)
 *      mutable t_gc: Mtime.span array;       (\* avg. time spent garbage collecting mail in reify (ns) *\)
 *      mutable memory: int64 array;     (\* measure in eat *\)
 *      mutable t_duration: Mtime.span }  (\* measure at start/end (seconds) *\) *)

let fresh_stat name arity event_count freq =
      { name = name;
        arity = arity;
        count = event_count;
        sample_freq = freq;
        n_tested = 0L;
        n_output = 0L;
        t_latency = 0.0;
        aux_n_latency = 0;
        aux_t_latency = Mtime.Span.zero;
        aux_c_latency = None;
        t_gc = 0.0;
        aux_t_gc = Mtime.Span.zero;
        aux_n_gc = 0;
        throughput = 0.0;
        memory = 0.0;
        n_memory_samples = 0;
        t_duration = Mtime.Span.zero }

let finalize stat counter =
  stat.t_latency  <- (Mtime.Span.to_ns stat.aux_t_latency) /. (float_of_int stat.aux_n_latency);
  stat.t_gc       <- (Mtime.Span.to_ns stat.aux_t_gc) /. (float_of_int stat.aux_n_gc);
  stat.memory     <- stat.memory /. (float_of_int stat.n_memory_samples);
  stat.t_duration <- Mtime_clock.count counter;
  stat.throughput <- (float_of_int (stat.arity * stat.count)) /. (Mtime.Span.to_s stat.t_duration)


let gc_time stat action =
  let start = Mtime_clock.now () in
  begin
    action ();
    stat.aux_n_gc <- stat.aux_n_gc + 1;
    stat.aux_t_gc <- Mtime.Span.add stat.aux_t_gc (Mtime.span (Mtime_clock.now ()) start)
  end

let mem_sample stat n thnk =
  if ((n mod stat.sample_freq) = 0) then
    begin
      stat.memory <- stat.memory +. (float_of_int (thnk ()));
      stat.n_memory_samples <- stat.n_memory_samples + 1
    end
  else ()

let begin_latency_sample stat n =
  match stat.aux_c_latency, (n mod stat.sample_freq) with
  | None, 0 ->
     stat.aux_c_latency <- Some (Mtime_clock.counter ())
  | _, _ -> ()

let end_latency_sample stat =
  match stat.aux_c_latency with
  | Some c ->
     stat.aux_n_latency <- stat.aux_n_latency + 1;
     stat.aux_t_latency <- Mtime.Span.add stat.aux_t_latency (Mtime_clock.count c);
     stat.aux_c_latency <- None
  | _ -> ()


effect InjectStat: t
let injectStat () = perform InjectStat
effect Terminate: 'a
let terminate () = perform Terminate

let csv_header = "name,arity,count,n_tested,n_output,t_latency_ns,t_gc_ns,throughput,memory,t_duration_s"
let to_csv_row stat =
  Printf.sprintf "\"%s\",\"%d\",\"%d\",\"%Ld\",\"%Ld\",\"%f\",\"%f\",\"%f\",\"%f\",\"%f\""
    stat.name
    stat.arity
    stat.count
    stat.n_tested
    stat.n_output
    stat.t_latency
    stat.t_gc
    stat.throughput
    stat.memory
    (Mtime.Span.to_s stat.t_duration)

type table = t array

let to_csv table =
  (String.concat "\n" (csv_header :: Array.(to_list (map to_csv_row table)))) ^ "\n"
