open Prelude
open Slot
open Core

module Tests = struct
  let s1 = mkSlot 0
  let s2 = mkSlot "0"
  let s3 = mkSlot 0.0
  let s4 = mkSlot 'c'
  let slots3 = Slots.(cons s1 @@ cons s2 @@ cons s3 @@ nil)
  let slots4 = Slots.(cons s4 @@ slots3)
  let list0 = Reactive.toR [evt 'x' (0,1);evt 'y' (1,2); evt 'z' (2,3)]
  let list1 = Reactive.toR [evt 1 (0,1); evt 2 (1,2)]
  let list2 = Reactive.toR [evt "A" (1,2); evt "B" (1,4)]
  let list3 = Reactive.toR [evt 0.3 (5,6); evt 0.2 (5,6); evt 0.1 (6,6)]
  let reacts3 = Reacts.(cons list1 @@ cons list2 @@ cons list3 nil)
  let reacts4 = Reacts.(cons list0 reacts3)
  let interleave3 = interleaved_bind slots3 reacts3
  let interleave4 = interleaved_bind slots4 reacts4

  module Three = (val DSL.mkJoinSig slots3)
  module Four = (val DSL.mkJoinSig slots4)
  module Join3 = JoinShape(Three)
  module Join4 = JoinShape(Four)

  let show3 action =
    let open Events in
    try action () with (* TODO: print intervals, too *)
    | effect (Join3.Trigger (S (i, S (s, S (f, Z))))) k ->
       continue k (Printf.printf "(%d,%s,%2.1f)\n" (payload i) (payload s) (payload f))

  let show4 action =
    let open Events in
    try action () with (* TODO: print intervals, too *)
    | effect (Join4.Trigger (S (c, S (i, S (s, S (f, Z)))))) k ->
       continue k (Printf.printf "(%c,%d,%s,%2.1f)\n" (payload c) (payload i) (payload s) (payload f))

  let test_join3 () =
    let open Handlers in
    ((Async.run) |+| show3 |+| Join3.run) interleave3

  let test_join4 () =
    let open Handlers in
    ((Async.run) |+| show4 |+| Join4.run) interleave4
end
