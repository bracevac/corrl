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
  type ('ctx,'res) q (* a query with binders 'ctx (which is a tuple of evt repr elements) yielding a 'res r repr.*)
  val from: 'a r repr -> ('a evt repr,'res) q
  val (@.): ('a,'b) q -> ('c,'b) q -> ('a * 'c,'b) q
  val correlate: ('a,'b) q -> ('a -> 'b evt repr) -> 'b r repr
  (*TODO: it seems rhiger's pattern combinators could be what we need*)
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
