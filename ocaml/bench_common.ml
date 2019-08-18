open Prelude
open Symantics
open Dsl2
open Stat

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

(* Global parameters *)
(* let repetitions = 10 *)
let repetitions = 1 (* TODO for testing *)
let samples = 10
let event_count = 1000
let flag_debug = true

let debug s =
  if flag_debug then println s else ()

let post_process measurements = measurements.(0) (* TODO *)

let write_csv title results =
  let tm = Unix.(gmtime (time ())) in
  let fname = Printf.sprintf "%s_%d%0*d%0*d%0*d%0*d%0*d.csv"
                title
                (tm.tm_year + 1900)
                2 (tm.tm_mon + 1)
                2 tm.tm_mday
                2 (tm.tm_hour + 2)
                2 tm.tm_min
                2 tm.tm_sec in
  let oc = open_out fname in
  output_string oc (to_csv results);
  close_out oc

let measure title instances =
  let num = Queue.length instances in
  let results = Array.init num (fun _ -> fresh_stat "" 0 0) in
  for r = 0 to (num - 1) do
    let (name,arity,join) = Queue.pop instances in
    Gc.compact ();
    let measurements = Array.init repetitions (fun _ -> fresh_stat name arity event_count) in
    for m= 0 to (repetitions - 1) do
      let stat = measurements.(m) in
      let t_start = now () in
      let _ = begin
          match println name with  (* TODO *)
          | () -> ()
          | effect InjectStat k -> continue k stat
          | effect Terminate _ -> ()
          end
      in
      let duration = now () -. t_start in
      stat.t_duration <- duration;
      stat.throughput <- (float_of_int (stat.arity * stat.count)) /. duration
    done;
    results.(r) <- post_process measurements
  done;
  write_csv title results
