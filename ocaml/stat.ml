(** Benchmarks statistics *)

type t =
  {  name: string;
     arity: int;        (* Arity *)
     count: int;        (* Number of events per stream *)
     sample_freq: int;  (* Sample frequency in number of events observed *)
     mutable n_tested: int;     (* Number of tuples tested against pattern  *)
     mutable n_output: int;     (* Number of tuples yielded *)
     mutable t_latency: float;  (* avg. latency *)
     mutable aux_n_latency: int; (* number of latency measurements  *)
     mutable aux_t_latency: float; (* current latency measurement *)
     mutable t_gc: float;       (* avg. time spent garbage collecting mail in reify *)
     mutable aux_n_gc: int;     (* number of gc time measurements *)
     mutable throughput: float; (* derivable by count/duration in the end *)
     mutable memory: float;     (* measure in eat *)
     mutable n_memory_samples: int;     (* measure in eat *)
     mutable t_duration: float }  (* measure at start/end *)



(*
count, memory utilization: in the eats/interleaved bind
latency: start in eat, end in reify, need aux var!


*)

let fresh_stat name arity event_count freq =
      { name = name;
        arity = arity;
        count = event_count;
        sample_freq = freq;
        n_tested = 0;
        n_output = 0;
        t_latency = 0.0;
        aux_n_latency = 0;
        aux_t_latency = -1.0;
        t_gc = 0.0;
        aux_n_gc = 0;
        throughput = 0.0;
        memory = 0.0;
        n_memory_samples = 0;
        t_duration = 0.0 }

(* TODO move it somewhere else *)
let now = Unix.gettimeofday

let gc_time stat action =
  let start = now () in
  begin
    action ();
    stat.aux_n_gc <- stat.aux_n_gc + 1;
    stat.t_gc <- stat.t_gc +. ((now () -. start))
  end

let mem_sample stat n thnk =
  if ((n mod stat.sample_freq) = 0) then
    begin
      stat.memory <- stat.memory +. (float_of_int (thnk ()));
      stat.n_memory_samples <- stat.n_memory_samples + 1
    end
  else ()

let begin_latency_sample stat n =
  if ((n mod stat.sample_freq) = 0) && (stat.aux_t_latency < 0.0) then
    stat.aux_t_latency <- now ()
  else ()

let end_latency_sample stat =
  if stat.aux_t_latency >= 0.0  then
    begin
      stat.aux_n_latency <- stat.aux_n_latency + 1;
      stat.t_latency <- stat.t_latency +. (now () -. stat.aux_t_latency);
      stat.aux_t_latency <- -1.0
    end
  else ()

effect InjectStat: t
let injectStat () = perform InjectStat
effect Terminate: 'a
let terminate () = perform Terminate

let csv_header = "name,arity,count,n_tested,n_output,t_latency,t_gc,throughput,memory,t_duration"
let to_csv_row stat =
  Printf.sprintf "\"%s\",\"%d\",\"%d\",\"%d\",\"%d\",\"%f\",\"%f\",\"%f\",\"%f\",\"%f\""
    stat.name
    stat.arity
    stat.count
    stat.n_tested
    stat.n_output
    stat.t_latency
    stat.t_gc
    stat.throughput
    stat.memory
    stat.t_duration

type table = t array

let to_csv table =
  String.concat "\n" Array.(to_list (map to_csv_row table))
