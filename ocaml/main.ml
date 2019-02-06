open Prelude
open Slot
open Core
open Symantics
open Dsl
open Restriction
open Hlists

module type TestSym = sig
  include JoinExtSym
  val lift: 'a elem repr list -> 'a shape repr
end

module type EvtTestSym = (TestSym with type 'a repr = 'a and type 'a elem = 'a evt and type 'a shape = 'a evt r)

module Tests(S: EvtTestSym) = struct
  open S
  let r0 = lift [evt 'x' (0,1);evt 'y' (1,2); evt 'z' (2,3)]
  let r1 = lift [evt 1 (0,1); evt 2 (1,2)]
  let r2 = lift [evt "A" (1,2); evt "B" (1,4)]
  let r3 = lift [evt 0.3 (5,6); evt 0.2 (5,6); evt 0.1 (6,6)]

  let runtime action = Async.run action

  let test_join3 () =
    join
      ((from r1) @. (from r2) @. (from r3) @. cnil)
      empty_ext
      (fun ((x1,_), ((x2,_), ((x3,_), ()))) ->
        yield (pair x1 (pair x2 x3)))

  let test_join4 () =
    join
      ((from r0) @. (from r1) @. (from r2) @. (from r3) @. cnil)
      empty_ext
      (fun ((x0,_), ((x1,_), ((x2,_), ((x3,_), ())))) ->
        yield (pair x0 (pair x1 (pair x2 x3))))
end

module CartesiusTests = struct
  (* TODO define syntax extensions for restriction handlers*)
  include Tests(Cartesius)
  open Cartesius
  let test_join4_most_recently () =
    let two   = Ptrs.n1 () in
    let three = Ptrs.n2 () in
    let four  = Ptrs.n3 () in
    join
      ((from r0) @. (from r1) @. (from r2) @. (from r3) @. cnil)
      ((most_recently four)
       |++| (most_recently two)
       |++| (most_recently three))
      (fun ((x0,_), ((x1,_), ((x2,_), ((x3,_), ())))) ->
        yield (pair x0 (pair x1 (pair x2 x3))))


end

let _ = CartesiusTests.(runtime test_join3)

(* let _ = Tests.test_join4_most_recently () *)
