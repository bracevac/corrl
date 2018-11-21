(* Type of promises *)
type 'a promise
(* Type of channels *)
type 'a channel
(* [async f] runs [f] concurrently *)
val async : (unit -> 'a) -> 'a promise
(* [await p] returns the result of the promise. *)
val await : 'a promise -> 'a
(* yields control to another task *)
val yield : unit -> unit
(* Runs the scheduler *)
val run   : (unit -> 'a) -> unit
val liftPromise : 'a -> 'a promise
val emit : 'a channel -> 'a -> unit
val receive: 'a channel -> 'a
val interleaved : (unit -> unit) array -> unit
