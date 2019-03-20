
(* This file shows a uniform way of defining both curried and uncurried polyvariadic joins. *)
module type Uncurried = sig
  type 'a repr
  type 'a shape
  type 'a pat

  type ('a,'b) ctx
  type ('a,'b) var

  val from: 'a shape repr -> ('a, 'a repr) var
  (* in the second type parameter of ctx, we keep track of the signature of the join pattern and the return type. we can only construct context values having types
(a1 * ... * an * unit, (a1 repr * ... an repr * unit -> res pat) * res)

 *)
  val cnil: (unit, (unit -> 'a pat) * 'a) ctx
  val (@.): ('a,'b) var -> ('c, ('d -> 'e) * 'f) ctx -> ('a * 'c, (('b * 'd) -> 'e) * 'f) ctx

  val yield: 'a repr -> 'a pat

  (* We can now have a single, uniform n-join signature. *)
  val join: ('shape, 'ps * 'res) ctx -> 'ps -> 'res shape repr
end

module TestUncurried(U: Uncurried) = struct
  open U
  let test a b c =
    join ((from a) @. (from b) @. (from c) @. cnil)
      (fun (x,(y,(z,()))) -> (yield y))
end

module type Curried = sig
  type 'a repr
  type 'a shape
  type 'a pat

  type ('a,'b) ctx
  type ('a,'b) var

  val from: 'a shape repr -> ('a, 'a repr) var
  val cnil: (unit, (unit -> 'a pat) * 'a) ctx
  (* In this formulation, the two styles differ only in the context concatenation, at the price
of having a final unit parameter in the curried version. *)
  val (@.): ('a,'b) var -> ('c, 'd * 'e) ctx -> ('a * 'c, ('b -> 'd) * 'e) ctx

  val yield: 'a repr -> 'a pat

  val join: ('shape, 'ps * 'res) ctx -> 'ps -> 'res shape repr
end

module TestCurried(C: Curried) = struct
  open C
  let test a b c =
    join ((from a) @. (from b) @. (from c) @. cnil)
      (fun x y z () -> (yield y))
end
