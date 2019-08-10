(** Benchmarks statistics *)

type t =
  {  name: string;
     arity: int;        (* Arity *)
     count: int;        (* Total number of events *)
     n_tested: int;     (* Number of tuples tested against pattern  *)
     n_output: int;     (* Number of tuples yielded *)
     t_latency: float;  (* avg. latency *)
     t_gc: float;       (* time spent garbage collecting in reify *)
     throughput: float; (* derivable by count/duration in the end *)
     memory: float;     (* measure in eat *)
     t_duration: float }  (* measure at start/end *)

let fresh_stat name arity event_count =
      { name = name;
        arity = arity;
        count = event_count;
        n_tested = 0;
        n_output = 0;
        t_latency = 0.0;
        t_gc = 0.0;
        throughput = 0.0;
        memory = 0.0;
        t_duration = 0.0 }

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
