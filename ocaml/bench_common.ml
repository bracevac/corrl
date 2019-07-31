open Prelude
open Slot
open Core
open Symantics
open Dsl
open Restriction
open Hlists

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

module type BenchSym = sig
  include JoinExtSym
  val lift: 'a elem repr list -> 'a shape repr
end

module type BenchSymantics = (BenchSym with type 'a repr = 'a and type 'a elem = 'a evt and type 'a shape = 'a evt r)

module CB = struct
  include Cartesius
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
