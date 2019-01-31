open Prelude
open Types


(* A slot x represents a binding 'x from ...' inside of a correlate block.
   Each binding has specific effects attached to it (generative effects).
   *)
module type YIELDFAIL = sig
  type t
  effect Yield: t evt -> unit
  val yield: t evt -> unit
  effect Fail: 'a
  val fail: unit -> 'a
end
type 'a yieldfail = (module YIELDFAIL with type t = 'a)
let mk_yieldfail (type s) (_: s typ) =
  (module struct
     type t = s
     effect Yield: t evt -> unit
     let yield evt = perform (Yield evt)
     effect Fail: 'a
     let fail () = perform Fail
   end: (YIELDFAIL with type t = s))
let mkYieldfail (type s) (t: s) = mk_yieldfail (witness t)

let fail_mod: type a b. a yieldfail -> b = fun yf ->
  let module YF = (val yf) in
  YF.fail ()

let yield_mod: type a. a yieldfail -> a evt -> unit = fun yf evt ->
  let module YF = (val yf) in
  YF.yield evt
