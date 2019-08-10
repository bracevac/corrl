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
     duration: float }  (* measure at start/end *)

let fresh_stat () =
  ref { name = "";
        arity = 0;
        count = 0;
        n_tested = 0;
        n_output = 0;
        t_latency = 0.0;
        t_gc = 0.0;
        (* t_react = 0.0;
         * t_latency = 0.0; *)
        throughput = 0.0;
        memory = 0.0;
        duration = 0.0 }

let csv_header = "name,arity,count,n_tested,n_output,t_latency,throughput,memory,duration"
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
    stat.duration

type table = t list
