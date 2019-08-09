open Prelude
open Slot
open Core
open Symantics
open Dsl2
open Restriction
open Hlists

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
let repetitions = 10
let samples = 10
let now = Unix.gettimeofday
let event_count = 1000
