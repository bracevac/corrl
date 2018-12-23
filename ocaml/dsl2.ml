module type Symantics = sig
  type 'a repr
  type 'a evt
  type 'a r
  type time

  val int: int -> int repr
  val bool: bool -> bool repr
  val string: string -> string repr
  val evt: 'a repr -> time repr -> 'a evt repr (*TODO necessary?*)
  val pair: 'a repr -> 'b repr -> ('a * 'b) repr

  val where: bool repr -> 'a evt repr -> 'a evt repr
  val yield: 'a repr -> 'a evt repr

  val time: 'a evt repr -> time repr
  val value: 'a evt repr -> 'a repr

  (* TODO should expose more operators in the language*)
  (* TODO: factor out the ops *)
  val (%>): time repr -> time repr -> bool repr

  (* (\* old uncurried version, requires tuples *\)
   * type 'a ctx
   * val from: 'a r repr -> 'b ctx -> ('a * 'b) ctx
   * val nil: unit ctx
   * (\* It seems acceptable to define a GADT for the purpose of variadic HOAS, because
   *    the shape will be statically known. *\)
   * type _ tuple =
   *   | TZ: unit tuple
   *   | TS: ('a evt repr * 'b tuple) -> ('a * 'b) tuple
   *   (\* Alternative that destructures the event into its components *\)
   *   (\* | TS: (('a repr * time repr) * 'b tuple) -> ('a * 'b) tuple *\)
   * (\* question: should we wrap the function in a repr, as in the HOAS examples from tagless paper? *\)
   * val correlate: 'a ctx -> ('a tuple -> 'b evt repr) -> 'b r repr *)

  (* New uncurried version, does not require an explicit tuple GADT for multiple binders. *)
  type 'a ctx (* a variable context of shape 'a (which is a tuple of evt repr elements) *)
  (* from binds a stream representation and introduces an anonymous variable representing an event from that stream.
     more precisely, a singleton variable context is introduced with that variable. *)
  val from: 'a r repr -> 'a evt repr ctx
  (* right-associative composition of two variable contexts *)
  val (@.): 'a ctx -> 'b ctx -> ('a * 'b) ctx
  (* forms a correlation pattern, given an 'b event expression with free variables/holes of shape 'a.
    The patterns yields a stream representation of 'b events.
    Can view the holed expression of type 'b as the product of an 'a ctx and
    an abstraction ('a -> 'b).
    Given that the ctx type is only formed by from and (@.) above, then
    we know that the variables have type 'a_i evt repr.
    Effectively, we get a curried variant of higher order abstract syntax, e.g.,
    'a ctx * ('a -> 'b repr) corresponds to an n-ary function taking a tuple  a_1 evt repr ... a_n evt repr
    to 'b repr. *)
  val correlate: 'a ctx -> ('a -> 'b evt repr) -> 'b r repr
  (*TODO: it seems rhiger's pattern combinators could be what we need*)

  val mouse: (int * int) r repr
  val key: int r repr
end

(* First order windows *)
module type WinSymantics = sig
  include Symantics
  val window: time repr -> time repr -> 'a r repr -> 'a r repr
end

(* Higher order windows *)
module type HigherWinSymantics = sig
  include Symantics
  val window: time repr -> time repr -> 'a r repr -> 'a r r repr
end

module Test(S:Symantics) = struct
  open S

  let exp a b c =
    correlate
      ((from a) @. (from b) @. (from c))
      (fun (x, (y, z)) ->
        where
          ((time x) %> (time y))
          (yield (pair (value x) @@ pair (value y) (value z))))

  (* let exp a b c = (* old, uncurried version *)
   *   correlate
   *     (from a
   *        (from b
   *           (from c nil)))
   *     (fun (TS (x, TS (y, TS (z, TZ)))) ->
   *       where
   *         (bool true)
   *         (yield (pair (value x) @@ pair (value y) (value z)))) *)
end

module type DeBruijnSymantics = sig
  type 'a ctx
  type 'a repr
  type 'a evt
  type 'a r
  type time

  (* Variables are lookup functions on a context, by construction, lookup always succeeds *)
  (*  type ('a,'b) var = 'b ctx -> 'a repr   *)
  val vz: ('a evt repr * _) ctx -> 'a evt repr
  val vs: ('r ctx -> 'a evt repr) -> (_ evt repr * 'r) ctx -> 'a evt repr

  val int: int -> 'a ctx -> int repr
  val (<%): ('a ctx -> int repr) -> ('a ctx -> int repr) -> 'a ctx -> bool repr
  val pair: ('a ctx -> 'b repr) -> ('a ctx -> 'c repr) -> 'a ctx -> ('b * 'c) repr
  (* val bool: bool -> bool repr
   * val string: string -> string repr
   * val evt: 'a repr -> time repr -> 'a evt repr (\*TODO necessary?*\)
   * val pair: 'a repr -> 'b repr -> ('a * 'b) repr *)

  val where: ('a ctx -> bool repr) -> ('a ctx -> 'b evt repr) -> 'a ctx -> 'b evt repr
  val yield: ('a ctx -> 'b repr) -> 'a ctx -> 'b evt repr

  val time: ('a ctx -> 'b evt repr) -> ('a ctx -> time repr)
  val value: ('a ctx -> 'b evt repr) -> ('a ctx -> 'b repr)
  (* val (%>): time repr -> time repr -> bool repr *)

  (* TODO it might make sense to make these context dependent too, e.g., for inlining optimizations  *)
  val from: 'a r repr -> ('a evt repr * unit) ctx
  val (@.): ('a * unit) ctx -> 'b ctx -> ('a * 'b) ctx
  val correlate: 'a ctx -> ('a ctx -> 'b evt repr) -> 'b r repr

  val mouse: (int * int) r repr
  val key: int r repr
end

module TestDeBruijn(S:DeBruijnSymantics) = struct
  open S

  (* 'a r repr ->
     'b r repr ->
     'c r repr -> ('a * ('b * 'c)) r repr  *)
  let q a b c =
    correlate
      ((from a) @. (from b) @. (from c))
       (where
          ((int 1) <% (int 2))
          (yield (pair (value vz) (pair (value (vs vz)) (value (vs (vs vz)))))))

  (* unit ->
     ((((int * int) evt * (int evt * (int * int) evt)) evt repr *
       ((int * int) evt repr * (int evt repr * unit)))
      ctx -> 'a evt repr) ->
      'a r repr   *)
  let q2 () =
    correlate
      ((from (q mouse key mouse)) @. (from mouse) @. (from key))
end
