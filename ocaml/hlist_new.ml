
module Nat = struct
  type z = Z : z
  type 'n s = S : 'n -> 'n s

  type (_,_) nat_leq =
    | LeqZero: (z, 'n) nat_leq
    | LeqSucc: ('n, 'm) nat_leq -> ('n s, 'm s) nat_leq

  type (_,_) nat_eq =
    | EqZero: (z, z) nat_eq
    | EqSucc: ('n, 'm) nat_eq -> ('n s, 'm s) nat_eq

  let rec eq_implies_leq: type n m. (n,m) nat_eq -> (n,m) nat_leq =
    (function
     | EqZero -> LeqZero
     | EqSucc pred -> LeqSucc (eq_implies_leq pred))
end

open Nat

(* Simple heterogeneous list GADT *)
module Hlist = struct
  type _ hlist =
    | [] : unit hlist
    | (::) : 'a * 'b hlist -> ('a * 'b) hlist
end

open Hlist

let test_hlist = [1; "2"; 3.0; '4']

module Project = struct
  (* The general pattern to define a nat-indexed family of combinators:*)
  (* 1. Nat-indexed type rel represents the *signature* of the combinator: (n, input, output) rel (see usage below).*)
  (* Usually, rel is an inductive definition. However, for nat-indexed combinators that are compositions
     of other nat-indexed combinators, rel is usually a product (cf. Split below).  *)
  type (_,_,_) rel =
    | Base: (z, 'a * 'b, 'a) rel
    | Step: ('n, 'a, 'b) rel -> ('n s, 'c * 'a, 'b) rel

  (* 2. Functions z, s construct signatures: applying s n-times to z yields the n-th signature. *)
  let z = Base
  let s n = Step n

  (* 3. The actual combinator that given (n, input, output) rel implements a function of type: input hlist -> output *)
  let rec p: type n a b. (n,a,b) rel -> a hlist -> b =
    (fun n hs ->
      match n, hs with
      | Base, (x :: _) -> x
      | (Step n), (_ :: hs) -> p n hs)

  let zeroth = p z test_hlist
  let once = p (s z) test_hlist
  let twice = p (s (s z)) test_hlist
  let thrice = p (s (s (s z))) test_hlist
  (* let frice  = p (s  (s (s (s z)))) test_hlist -- error *)
end

module Take = struct
  type (_,_,_) rel =
    | Base: (z,'a, unit hlist) rel
    | Step: ('n, 'a, 'b hlist) rel -> ('n s, 'c * 'a, ('c * 'b) hlist) rel

  let z = Base
  let s n = Step n

  let rec p: type n a b. (n,a,b) rel -> a hlist -> b =
    (fun n hs ->
      match n, hs with
      | Base, _ -> []
      | (Step n), (x :: hs) -> x :: (p n hs))


  let zeroth = p z
  let once = p (s z)
  let twice = p (s (s z))
  let thrice = p (s (s (s z)))

end

module Drop = struct
  type (_,_,_) rel =
    | Base: (z,'a, 'a hlist) rel
    | Step: ('n, 'a, 'b hlist) rel -> ('n s, 'c * 'a, 'b hlist) rel

  let z = Base
  let s n = Step n

  let rec p: type n a b. (n,a,b) rel -> a hlist -> b =
    (fun n hs ->
      match n, hs with
      | Base, hs -> hs
      | (Step n), (_ :: hs) -> p n hs)


  let zeroth = p z
  let once = p (s z)
  let twice = p (s (s z))
  let thrice = p (s (s (s z)))
end

(* A product of the three above constructions. Could we use hlists to
make this polyvariadic, too?  *)
module Split = struct
  type (_,_,_) rel =
    | Prod: ('n,'a,'b) Take.rel
             * ('n,'a,'c) Project.rel
             * ('n s,'a,'d) Drop.rel -> ('n,'a,'b * 'c * 'd) rel

  let z = Prod (Take.Base, Project.Base, Drop.(Step Base))
  let s (Prod (t,p,d)) = Prod (Take.Step t, Project.Step p, Drop.Step d)

  let p: type n a b. (n,a,b) rel -> a hlist -> b =
    (fun (Prod (tn,pn,dn)) hs -> (Take.p tn hs, Project.p pn hs, Drop.p dn hs))

  let zeroth = p z
  let once = p (s z)
  let twice = p (s (s z))
  let thrice = p (s (s (s z)))
end

(*TODO: can we have this in final tagless?*)
module Rev = struct
  type (_,_,_) rel =
    | Base: (z, (unit * 'a),'a hlist) rel
    | Step: ('n, 'b * ('a * 'c),'d hlist) rel -> ('n s, ('a * 'b) * 'c,'d hlist) rel

  let z = Base
  let s n = Step n

  let rec aux: type n a b c. (n, (a * b), c) rel -> b hlist -> a hlist -> c =
    (fun rel acc hs ->
      match rel, hs with
      | Base, _ -> acc
      | Step n, x :: xs -> aux n (x :: acc) xs)

  let p: type n a b. (n, (a * unit), b) rel -> a hlist -> b =
    (fun rel hs -> aux rel [] hs)

  let zeroth = p z
  let once   = p (s z)
  let twice  = p (s (s z))
  let thrice = p (s (s (s z)))
end

(* module type Variadic = sig
 *   type ('n,'i,'o) rel
 *   type ('i,'o) zero_in
 *   type ('i,'o) zero_out
 *   type ('n,'i,'o) succ_in
 *   type ('n,'i,'o) succ_out
 *   val z: (z, ('i, 'o) zero_in, ('i, 'o) zero_out) rel
 *   val s: ('n, 'i, 'o) rel -> ('n s, ('n,'i,'o) succ_in, ('n,'i,'o) succ_out) rel
 *   val apply: ('n, 'i, 'o) rel -> 'i hlist -> 'o
 * end
 *
 * module Product(V1: Variadic)(V2: Variadic) = struct
 *   type (_,_,_) rel =
 *     | Prod: ('n, 'i, 'o1) V1.rel
 *           * ('n, 'i, 'o2) V2.rel -> ('n, 'i, 'o1 * 'o2) rel
 *
 *   type ('i, 'o1 * 'o2) zero_in = ('i, 'o1) V1.zero_in * ('i, 'o1) V2.zero_in
 *   type ('i, 'o) zero_out = ('i, 'o) V1.zero_out * ('i, 'o) V2.zero_out
 *
 *
 * end *)


module RangeSplit = struct

  (* this seems wrong, because we want the number index to reflect the total number of
   * elements instead of the current element
   * type (_,_,_) rel =
   *   | Base: (z, 'a, 'b) Split.rel -> (z, 'n * 'a, 'b hlist) rel
   *   | Step: (('m, 'n * 'a, 'b hlist) rel
   *            * ('m s,'n) nat_leq
   *            * ('m s, 'a, 'c) Split.rel)
   *           -> ('m s, 'n * 'a, ('c * 'b) hlist) rel *)

  (* 1st variant: count upwards. It should not be possible that the counter exceeds the max. number of splits. *)
  type (_,_,_) iter =
    (* input parameters: 'n: how many elements left, 'a: the input hlist   *)
    | Start: (z, 'a, 'b) Split.rel -> (z, 'n * 'a, 'b) iter
    | Inc: ('i, 'n s * 'a, 'b) iter
            * ('i s, 'a, 'c) Split.rel
            -> ('i, 'n * 'a, 'c * 'b) iter

  (* (\* Dually, 2nd variant: count downwards. *\)
   * type (_,_,_) iter =
   *   | Stop: ('n, 'a, 'b) Split.rel -> ('n, z * 'a, 'b) iter
   *   | Dec: ('i s, 'n * 'a, 'b) iter
   *           * ('i, 'a, 'c) Split.rel
   *           -> ('i, 'n s * 'a, 'c * 'b) iter *)




  (* type (_,_,_) rel =
   *   | Iter: ('n, z * 'a, 'c) iter -> ('n, 'a, 'c hlist) rel *)

  (* Q: How should zero and succ be constructed? And: It should be sufficient to construct iter instances without us
     spelling them out manually. Seems we may need to reify actual number values. *)

  type (_,_,_) rel' =
    | Base: (z, 'a, 'b) Split.rel -> (z, 'a, ('b * unit) hlist) rel'
    | Step: ('n, 'a, 'b hlist) rel' * ('n s, 'a, 'c) Split.rel -> ('n s, 'a, ('c * 'b) hlist) rel'

  type (_,_,_) rel =
    | Prod: ('n, 'a, ('b * 'c) hlist) rel' * ('n, 'a, 'b) Split.rel -> ('n, 'a, ('b * 'c) hlist) rel

  let z k = k (Prod (Base Split.z, Split.z))
  let s n k =
    n (function
       | Prod (x, splitn) ->
          Prod (Step (x,  )
      )








  (* let zeroth =  z
   * let once =  (s z)
   * let twice =  (s (s z))
   * let thrice =  (s (s (s z))) *)





  (* type (_,_,_) rel =
   *   | Prod: (('n, 'a, 'b) aux
   *            * ('n,
   *
   *
   *   | Base: (z, 'a, 'b) Split.rel -> (z, 'a, 'b hlist) rel
   *   | Step: (('n, 'a, 'b hlist) rel * ('n s, 'a, 'c) Split.rel) ->
   *           ('n s, 'a, ('c * 'b) hlist) rel
   *
   * let z = Base Split.z
   * let s n =   *)
end

module Unfold = struct
end

module AccSplit = struct

end

(* Idea: could we have a single push effect with a GADT modelling the different positions?
Of course, the price to pay is the boilerplate for daisy-chaining the ops *)
