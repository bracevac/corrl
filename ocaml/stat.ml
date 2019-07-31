(** Benchmarks statistics *)
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
