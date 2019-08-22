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

let rand_array n =
  Array.init n (fun i -> (Ev (Random.int 1073741823, (i,i))))

(* Global parameters *)
(* let repetitions = 10 *)
let repetitions = 2 (* TODO for testing *)
let samples = 100
let event_count = 10000 (* TODO: should be distributed evenly*)
let flag_debug = true

let debug s =
  if flag_debug then println s else ()

let write_csv title name results =
  let tm = Unix.(gmtime (time ())) in
  let suffix = Printf.sprintf "%d%0*d%0*d%0*d%0*d%0*d.csv"
                (tm.tm_year + 1900)
                2 (tm.tm_mon + 1)
                2 tm.tm_mday
                2 (tm.tm_hour + 2)
                2 tm.tm_min
                2 tm.tm_sec in
  let fname i = Printf.sprintf "%s_%s_%i_%s" title name i suffix in
  Array.iteri (fun i stat ->
      let oc = open_out (fname i) in
      output_string oc (to_csv stat);
      close_out oc) results

let measure title instances =
  let _ = Printf.printf "%s\n%!" title in
  let num = Queue.length instances in
  for r = 0 to (num - 1) do
    let (name,arity,join) = Queue.pop instances in
    Printf.printf "%s: setup\n%!" name;
    let measurements = Array.init repetitions (fun _ -> fresh_stat name arity event_count samples) in
    for m = 0 to (repetitions - 1) do
      Gc.compact ();
      let stat = measurements.(m) in
      let j = try join () with (* Important to avoid measuring setup overhead *)
              | effect InjectStat k -> continue k stat
      in
      let _ = Printf.printf "%s: start\n%!" name in
      let counter = Mtime_clock.counter () in
      let _ =  try Async.run j with
               | Stack_overflow ->
                  Printf.printf "Warning: Stack_overflow in %s\n%!" name;
                  Printexc.print_backtrace stdout
               | Failure m -> Printf.printf "Warning: Failure '%s' in %s\n%!" m name;
                              Printexc.print_backtrace stdout
               | effect InjectStat k -> continue k stat
               | effect Terminate _ -> ()
      in
      finalize stat counter;
      Printf.printf "%s: end\n%!" name
    done;
    write_csv title name measurements
  done;
  Printf.printf "%s: done\n%!" title
