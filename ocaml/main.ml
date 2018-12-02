open Prelude
open Slot
open Core
open Dsl
open Restriction

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

  let j3 = Dsl.mkJoinSig slots3
  let j4 = Dsl.mkJoinSig slots4
  module J3 = (val j3)
  module J4 = (val j4)
  module Join3 = JoinShape(J3)
  module Join4 = JoinShape(J4)

  let printer (type a) (j: a join_sig) (show: a Events.hlist -> string) action =
    let module J = (val j) in
    try action () with
    | effect (J.Trigger tuple) k -> continue k (print_string  (show tuple))

  let show3 =
    (* TODO: print intervals, too *)
    let show Events.((S (i, S (s, S (f, Z))))) =
      Printf.sprintf "(%d,%s,%2.1f)\n" (payload i) (payload s) (payload f)
    in printer j3 show

  let show4 =
    (* TODO: print intervals, too *)
    let show Events.(S (c, S (i, S (s, S (f, Z))))) =
      Printf.sprintf "(%c,%d,%s,%2.1f)\n" (payload c) (payload i) (payload s) (payload f)
    in printer j4 show

  let runtime action = Async.run action

  let test_join3 () =
    (runtime |+| show3)
      (correlate (s (s (s z))) ~with_sig: j3
         list1
         list2
         list3)

  let test_join4 () =
    (runtime |+| show4)
      (correlate (s (s (s (s z)))) ~with_sig: j4
         list0
         list1
         list2
         list3)

  let test_join4_most_recently () =
    let two = Hlists.(Next Here) in (* TODO use nice combinators instead of raw constructors *)
    let three = Hlists.(Next (Next Here)) in
    let four =  Hlists.(Next (Next (Next Here))) in
    let computation () =
    (* for now this is the manual expansion of the system *)
      Async.async (fun () -> (* Important: each join runs in a separate async strand *)
          (show4 |+| Join4.((memory ())
                            |+| (reify ())
                            |+| (most_recently four J4.slots)
                            |+| (most_recently two J4.slots) (* TODO: implement the injection mechanism *)
                            |+| (most_recently three J4.slots)
                            |+| (forAll ()))) interleave4)
    in runtime computation (* TODO now integrate this with the correlate combinator *)
end

let _ = Tests.test_join4_most_recently ()
