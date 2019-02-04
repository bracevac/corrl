open Prelude

(* A generative effect for yielding/failing in joins. *)
module type YIELDFAIL = sig
  type t
  effect Yield: t evt -> unit
  val yield: t evt -> unit
  effect Fail: 'a
  val fail: unit -> 'a
end
type 'a yieldfail = (module YIELDFAIL with type t = 'a)
let mk_yieldfail: type s. unit -> s yieldfail = fun () ->
  (module struct
     type t = s
     effect Yield: t evt -> unit
     let yield evt = perform (Yield evt)
     effect Fail: 'a
     let fail () = perform Fail
   end: (YIELDFAIL with type t = s))

let fail_mod: type a b. a yieldfail -> b = fun yf ->
  let module YF = (val yf) in
  YF.fail ()

let yield_mod: type a. a yieldfail -> a evt -> unit = fun yf evt ->
  let module YF = (val yf) in
  YF.yield evt
