
module Nat = struct
  type z = Z : z
  type 'n s = S : 'n -> 'n s

  type (_,_) nat_leq =
    | ZeroLeq: (z, 'n) nat_leq
    | SuccLeq: ('n, 'm) nat_leq -> ('n s, 'm s) nat_leq

  type (_,_) nat_eq =
    | ZeroEq: (z, z) nat_eq
    | SuccEq: ('n, 'm) nat_eq -> ('n s, 'm s) nat_eq

  let rec eq_implies_leq: type n m. (n,m) nat_eq -> (n,m) nat_leq =
    (function
     | ZeroEq -> ZeroLeq
     | SuccEq pred -> SuccLeq (eq_implies_leq pred))
end

open Nat

(* Simple heterogeneous list GADT *)
module Hlist = struct
  type _ hlist =
    | [] : unit hlist
    | (::) : ('a * 'b hlist) -> ('a * 'b) hlist
end

open Hlist

let test_hlist = [1; "2"; 3.0; '4']

module Project = struct
  type (_,_,_) rel =
    | Base: (z, 'a * 'b, 'a) rel
    | Step: ('n, 'a, 'b) rel -> ('n s, 'c * 'a, 'b) rel

  let z = Base
  let s n = Step n

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
    | Prod: (('n,'a,'b) Take.rel
             * ('n,'a,'c) Project.rel
             * ('n s,'a,'d) Drop.rel) -> ('n,'a,'b * 'c * 'd) rel

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
    | Step: ('n,('b * ('a * 'c)),'d hlist) rel -> ('n s,(('a * 'b) * 'c),'d hlist) rel

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

module RangeSplit = struct
  type (_,_,_,_) acc =
    | AccZ: (z, 'a, 'b) Split.rel -> ('n, z, 'a, 'b hlist) acc
    | AccS:
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
