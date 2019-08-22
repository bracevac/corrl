(** Benchmarks statistics *)


type t =
  {  name: string;
     arity: int;        (* Arity *)
     count: int;        (* Total number of events, will be divided evenly *)
     sample_freq: int;  (* Sample frequency in number of events observed *)
     mutable cursor: int; (* how many of the #count events have been pushed so far?*)
     mutable n_tested: int64;     (* Number of tuples tested against pattern  *)
     mutable n_output: int64;     (* Number of tuples yielded *)
     t_latency: Mtime. Queue.t;
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
        aux_t_gc = Mtime.Span.zero;
        memory = Queue.create ();
        t_duration = Mtime.Span.zero }

let finalize stat counter =
  stat.t_duration <- Mtime_clock.count counter


let gc_time stat action =
  let start = Mtime_clock.now () in
  begin
    action ();
    Queue.push (Mtime.span (Mtime_clock.now ()) start) stat.t_gc
  end

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
  match stat.aux_c_latency with
  | Some (cursor, n_out, c) when (stat.cursor - cursor) >= stat.aux_throughput_duration ->
     let time = Mtime.Span.to_s (Mtime_clock.count c) in
     let delta = Int64.(to_float (sub stat.n_output n_out)) in
     Queue.push (delta /. time) stat.t_latency;
     stat.aux_c_latency <- None
  | _ -> ()



effect InjectStat: t
let injectStat () = perform InjectStat
effect Terminate: 'a
let terminate () = perform Terminate

let misc_row stat =
   Printf.sprintf "\"%s\",\"%d\",\"%d\",\"%Ld\",\"%Ld\",\"%f\"\n"
     stat.name
     stat.arity
     stat.count
     stat.n_tested
     stat.n_output
     (Mtime.Span.to_s stat.t_duration)

let latency_row stat =
  let data =  stat.name :: (Int.to_string stat.arity)  :: (Int.to_string stat.count) :: Queue.(to_list (map (fun x -> Float.to_string (Mtime.Span.to_ns x)) stat.t_latency)) in
  (String.concat "," data) ^ "\n"

let throughput_row stat =
  let data =  stat.name :: (Int.to_string stat.arity)  :: (Int.to_string stat.count) :: Queue.(to_list (map (Float.to_string) stat.t_throughput)) in
  (String.concat "," data) ^ "\n"

let gc_row stat =
  let data =  stat.name :: (Int.to_string stat.arity)  :: (Int.to_string stat.count) :: Queue.(to_list (map (fun x -> Float.to_string (Mtime.Span.to_ns x)) stat.t_gc)) in
  (String.concat "," data) ^ "\n"

let memory_row stat =
  let data =  stat.name :: (Int.to_string stat.arity)  :: (Int.to_string stat.count) :: Queue.(to_list (map (Int.to_string) stat.memory)) in
  (String.concat "," data) ^ "\n"
