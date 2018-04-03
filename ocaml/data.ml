(* Time models are monoids over some time representation *)
module type TIMEMODEL = sig
  type time
  val ( |@| ) : time -> time -> time
  val tzero : time
end

(* An event is evidence of something that happened at a specific time *)
module type EVENT = sig
  module Time : TIMEMODEL
  type 'a evt = Ev of 'a * Time.time
end

module Event(T: TIMEMODEL): (EVENT with module Time = T) = struct
  module Time = T
  type 'a evt = Ev of 'a * T.time
end

(* Our default time model is interval-based *)
module Interval : (TIMEMODEL with type time = int * int) = struct
  type time = int * int
  let ( |@| ) (a,b) (c,d) = (min a c, max b d)
  let tzero = (max_int, min_int) (* representation of empty interval *)
end

module Evt = Event(Interval)

(* Event streams *)
module Reactive = struct
  type 'a react = RNil | RCons of 'a * ('a react Async.promise)
  type 'a r = 'a react Async.promise

  let toR: 'a list -> 'a r = fun l ->
    let rec _toR =
    function
    | [] -> RNil
    | x::xs -> RCons (x, (Async.liftPromise (_toR xs)))
    in Async.liftPromise (_toR l)

  let liftArray: 'a array -> 'a r = fun a ->
    Async.liftPromise (Array.fold_right (fun x -> fun r -> RCons (x, (Async.liftPromise r))) a RNil)

  let rec eat f stream =
    match Async.await stream with
    | RCons (hd, tl) -> (f hd); eat f tl
    | RNil -> ()
end

(** Models a type for lifetime counters, which is either a finite int value or infinity. *)
module Count = struct
  (* TODO prohibit negative values *)
  type t = Inf | Fin of int
  let map f = function
    | Inf -> Inf
    | Fin i -> Fin (f i)
  let flatMap f = function
    | Inf -> Inf
    | Fin i -> (f i)
  let inc n = map (fun i -> i + 1) n
  let inc_by num n = map (fun i -> i + num) n
  let dec n = map (fun i -> i - 1) n
  let dec_by num n = map (fun i -> i - num) n
  let add n m = flatMap
                  (fun i ->
                    map (fun j -> i + j) m) n
  let sub n m = flatMap
                  (fun i ->
                    map (fun j -> i - j) m) n
  let lt n m = match (n,m) with
    | (_, Inf) -> true
    | (Fin i, Fin j) -> i < j
    | _ -> false
  let lte n m = (n = m) || (lt n m)
  let gt n m = not (lte n m)
  let gte n m = (n = m) || (gt n m)
  let lt_i i n = lt (Fin i) n
  let lte_i i n = lte (Fin i) n
  let gt_i i n = gt (Fin i) n
  let gte_i i n = gte (Fin i) n
  let inc_snd n l = List.map (fun (y,c) -> (y, (add c n))) l
  let dec_snd n l = List.map (fun (y,c) -> (y, (sub c n))) l
end
