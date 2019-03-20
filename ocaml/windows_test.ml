open Hlists

module type Foo = sig
  type 'a repr
  type 'a shape

  type ('a,'b) ctx
  type ('a,'b) var
  val from: 'a shape repr -> ('a, 'a) var
  val cnil: (unit, unit) ctx
  val (@.): ('a,'b) var -> ('c,'d) ctx -> ('a * 'c, 'b * 'd) ctx

  type 'a body
  type ('a,'b,'c) pat

  val yield: 'a repr -> 'a body
  val where: bool repr -> 'a body -> 'a body

  val bool: bool -> bool repr
  val pair: 'a repr -> 'b repr -> ('a * 'b) repr

  val pabs: ('pvars -> 'a body) -> ('ctxshape, 'pvars, 'a) pat
  val window: int -> ('ctxshape, 'pvars, 'a) pat -> ('ctxshape, 'pvars, 'a) pat

  val join: ('shape,'pvars) ctx -> ('shape,'pvars,'res) pat -> 'res shape repr

  (* it seems we need pointers/pointer multisets to support disjunctions *)

  (* Join calculus style triangle for disjunction clause *)
  type ('ctx,'res) clause
  val (|>|): ('local,'global) Ptrs.hlist -> ('local -> 'res body) -> ('global,'res) clause

  (* TODO: how would a unification of patterns and disjunctions look like? *)

  (* Another question: should all pattern cases have same result type?
     Is that even useful? Or should it be a group of heterogeneous branches?
     (Seems more useful). With n-ary sums, they are equivalent (though one of them looks "less embedded"). *)
  (* Pattern disjunction, uniform branch type. *)

  val (|.): ('ctx,'res) clause -> ('ctx, 'res) clause -> ('ctx, 'res) clause
  val disj: ('ctx, 'res) clause -> ('ctx, 'pvars, 'res) pat
end

module TestFoo(F: Foo) = struct
  open F
  let test a b c =
    join ((from a) @. (from b) @. (from c) @. cnil)
      (window 1 (pabs (fun (x,(y,(z,()))) ->
                     where (bool true)
                       (yield (pair z (pair x y))))))

  let (@&) a b = Ptrs.(cons a b)

  let test2 a b c d =
    let open Ptrs in
    join ((from (test a b c)) @. (from b) @. (from c) @. (from d) @. cnil)
      (disj
         (((n0 @& n2 @& nil ()) |>| (fun (x,(y,())) -> yield (bool true)))
       |. ((n1 @& n2 @& n3 @& nil ()) |>| (fun (x,(y,(z,()))) -> yield (bool false)))
       |. ((n3 @& nil ()) |>| (fun (z,()) -> yield (bool false))) ))


end
