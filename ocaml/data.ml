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
  val evt: 'a -> Time.time -> 'a evt
  val payload: 'a evt -> 'a
  val time: 'a evt -> Time.time
end

module Event(T: TIMEMODEL): (EVENT with module Time = T) = struct
  module Time = T
  type 'a evt = Ev of 'a * T.time
  let evt x i = Ev (x,i)
  let payload (Ev (x,_)) = x
  let time (Ev (_,t)) = t
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
    Async.liftPromise (Array.fold_right (fun x r -> RCons (x, (Async.liftPromise r))) a RNil)

  let create: unit -> 'a r = Async.promise
  let resolve_next: 'a r -> 'a -> 'a r = fun react v ->
    let next = create () in
    Async.resolve react (RCons (v,next));
    next
  let rec eat f stream =
    match Async.await stream with
    | RCons (hd, tl) -> (f hd); eat f tl
    | RNil -> ()
end

include Count

type 'a mailbox = ('a Evt.evt * (Count.t ref)) list
