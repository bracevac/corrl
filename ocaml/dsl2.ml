(* module Test(S:Symantics) = struct
 *   open S
 *
 *   let exp a b c =
 *     correlate
 *       ((from a) @. (from b) @. (from c))
 *       (fun (x, (y, z)) ->
 *         where
 *           ((time x) %> (time y))
 *           (yield (pair (value x) @@ pair (value y) (value z))))
 *
 *   (\* let exp a b c = (\* old, uncurried version *\)
 *    *   correlate
 *    *     (from a
 *    *        (from b
 *    *           (from c nil)))
 *    *     (fun (TS (x, TS (y, TS (z, TZ)))) ->
 *    *       where
 *    *         (bool true)
 *    *         (yield (pair (value x) @@ pair (value y) (value z)))) *\)
 * end
 *
 * (\* A DeBruijn version of event variables. We have to explicitly thread variable contexts through the
 *    pattern body *\)
 * module type DeBruijnSymantics = sig
 *   type 'a ctx
 *   type 'a repr
 *   type 'a evt
 *   type 'a r
 *   type time
 *
 *   (\* Correlation patterns are expressions with free event variables in context 'a yielding
 *      a result of type 'b. *\)
 *   type ('a,'b) pat = 'a ctx -> 'b repr
 *
 *   (\* Variables are lookup functions on a context, by construction, lookup always succeeds *\)
 *   (\* This variant separates variable formation from the pattern expressions.
 *      Erasing this distinction would enable, e.g., explicit lifting of an expression into
 *      a larger variable context via successor. We do not require such flexibility for now. *\)
 *   type ('a, 'b) var
 *   val vz: ('a evt repr * _, 'a evt) var
 *   val vs: ('r, 'a evt) var -> (_ evt repr * 'r, 'a evt) var
 *   val (?!): ('a,'b) var -> ('a,'b) pat
 *
 *   val int: int -> ('a, int) pat
 *   val (<%): ('a, int) pat -> ('a, int) pat -> ('a, bool) pat
 *   val pair: ('a, 'b) pat -> ('a, 'c) pat -> ('a, ('b * 'c)) pat
 *   (\* val bool: bool -> bool repr
 *    * val string: string -> string repr
 *    * val evt: 'a repr -> time repr -> 'a evt repr (\\*TODO necessary?*\\)
 *    * val pair: 'a repr -> 'b repr -> ('a * 'b) repr *\)
 *
 *   val where: ('a, bool) pat -> ('a, 'b evt) pat -> ('a, 'b evt) pat
 *   val yield: ('a, 'b) pat -> ('a, 'b evt) pat
 *   val time: ('a, 'b evt) pat -> ('a, time) pat
 *   val value: ('a, 'b evt) pat -> ('a, 'b) pat
 *
 *   (\* val (%>): time repr -> time repr -> bool repr *\)
 *
 *   (\* context formation *\)
 *   (\* a single binder, binding a reactive to an event variable of the same element type 'a. *\)
 *   val from: 'a r repr -> ('a evt repr * unit) ctx
 *   (\* extend context by one binding, right associative *\)
 *   val (@.): ('a * unit) ctx -> 'b ctx -> ('a * 'b) ctx
 *
 *   val correlate: 'a ctx -> ('a, 'b evt) pat -> 'b r repr
 *
 *   val mouse: (int * int) r repr
 *   val key: int r repr
 * end
 *
 * module TestDeBruijn(S:DeBruijnSymantics) = struct
 *   open S
 *
 *   (\* 'a r repr ->
 *      'b r repr ->
 *      'c r repr -> ('a * ('b * 'c)) r repr  *\)
 *   let q a b c =
 *     correlate
 *       ((from a) @. (from b) @. (from c))
 *        (where
 *           ((int 1) <% (int 2))
 *           (yield (pair (value ?!vz) (pair (value ?!(vs vz)) (value ?!(vs (vs vz)))))))
 *
 *   (\* unit ->
 *      ((((int * int) evt * (int evt * (int * int) evt)) evt repr *
 *        ((int * int) evt repr * (int evt repr * unit)))
 *       ctx -> 'a evt repr) ->
 *       'a r repr   *\)
 *   let q2 () =
 *     correlate
 *       ((from (q mouse key mouse)) @. (from mouse) @. (from key))
 *
 *   let q3 x =
 *     let r1 =
 *       correlate
 *         ((from mouse) @. (from mouse) @. (from mouse))
 *         (where
 *            ((int 1) <% (int 2))
 *            (yield (pair (value ?!vz) (pair (value ?!(vs vz)) (value ?!(vs (vs vz)))))))
 *     in let r2 =
 *          correlate
 *            ((from r1) @. (from x))
 *            (yield (value ?!vz))
 *        in (r1, r2)
 * end *)


(* module type Sym = sig
 *   type 'a repr
 *   type 'a shape
 *   type 'a meta
 *   (\* Elements come out of shapes. they consist of a value and meta data (e.g., time, coordinate, etc.) *\)
 *   type 'a el_repr = 'a repr * 'a meta repr
 *
 *   (\*TODO: the design of where is also awkward
 *     Either we let
 *     a the syntax of join patterns describe the shape representatin (as suzuki et al.). consequence:
 *       require allocation of containers per yielded tuple, notion of empty container
 *       (-> would fit into cartesius)
 *
 *     b the syntax of join patterns describe element representation. consequence:
 *       where syntax requires encoding explicit notion of failure, or notion of empty/invalid el_repr
 *
 *       introduction always works:
 *       'a repr * 'a meta repr -> 'a elem repr
 *
 *       elimination *does not*:
 *       'a elem repr -> 'a repr * 'a meta repr
 *       because it is not well-defined for fail.
 *
 *       it seems ok, as long as the concrete symantics includes failure mechanism
 *       (cartesius does it)
 *
 *   *\)
 *   val where: bool repr -> 'a el_repr -> 'a el_repr
 *
 *   (\* also awkward: how to compute the time data? need access to context here.
 *      there is good reason to not expose the time computation of the complex events
 *      in the notation: verbose, potential error source.
 *
 *      Perhaps we could have an effect signature as part of the dsl?
 *
 *      yield: 'a repr -> <ctx> 'a el_repr
 *
 *      Of course, the problem is that there is no way of tracking the effect types.
 *      Perhaps, we could try to manually emulate a primitive type and effect system?
 *      type ('a,'e,'b) eff = 'a -> 'b
 *      (\* Problem: limited capabilities to express effect rows, e.g., removal in the middle, permutation, etc. *\)
 *      val handle: ('a,'e,'b) eff -> 'e handle -> ('a, unit, 'b) eff
 *      val lift: ('a -> 'b) -> ('a, unit, 'b) eff
 *      val unwrap: ('a, unit, 'b) eff -> 'a -> 'b
 *
 *      yield's implementation somehow has to get ahold of the implicit time data,
 *      without explicit passing.
 *
 *
 *      Also, what would be the type of the ctx effect in the yield form?
 *      it is dependent on the context! Could we use our gadt-based trick?
 *
 *      In our specific case, the meta data is not dependent on the variable types.
 *      This hugely simplifies the design.
 *      However, what in the general case? Injection via effects requires determining
 *      the injection effect's signature somehow, per instance.
 *      However, do we actually need type-dependent meta data? What would it look like?
 *
 *
 *  *\)
 *   val yield: 'a repr -> 'a el_repr
 *
 *   type 'a ctx
 *   type 'a var
 *   val from: 'a shape repr -> 'a el_repr var
 *   val cnil: unit ctx
 *   val (@.): 'a var -> 'b ctx -> ('a * 'b) ctx
 *
 *   val join: 'a ctx -> ('a -> 'b el_repr) -> 'b shape repr
 * end *)

module type SymPat = sig
  type 'a repr
  type 'a shape
  type meta
  type 'a el_repr = 'a repr * meta repr

  type ('s,'a) ctx
  type ('s,'a) var
  val from: 'a shape repr -> ('a,'a el_repr) var
  val cnil: (unit, unit) ctx
  val (@.): ('u, 'a) var -> ('s,'b) ctx -> (('u * 's), ('a * 'b)) ctx

  type ('ctx,'a) pat (* A pattern with result 'a el_repr and implicit metadata context 'ctx *)
  (* it seems even better to abstract the type via pat. this way, we can choose
    whether patterns are element-level or collection-level specifications:
    ('a, 'ctx) pat = 'a el_repr
    ('a, 'ctx) pat = 'a shape repr  *)

  val pair: 'a repr -> 'b repr -> ('a * 'b) repr
  val bool: bool -> bool repr

  val where: bool repr -> ('m,'a) pat -> ('m,'a) pat
  val yield: 'a repr -> ('m,'a) pat

  val join: ('s,'a) ctx -> ('a -> ('s,'b) pat) -> 'b shape repr
end

module type InjectSymantics' = sig
  include SymPat
  type 'ctx ext
  val empty_ext: 'ctx ext
  val (|++|): 'ctx ext -> 'ctx ext -> 'ctx ext

  (* 1. the injectable resources are built from the available variable context, how is the latter communicated?
    By propagating it in the type of the expression with the hole! *)
  (* 2. it does not seem particularly useful to inject dependent resources at a separate point from the join,
    rather, the join form should have an extra parameter...   *)

  type ('h,'a) open_shape

  val join: ('s,'a) ctx -> ('a -> ('s,'b) pat) -> ('s,'b) open_shape

  val inject: 'a ext -> ('b, 'a) open_shape -> 'b shape repr

  (*
    The dptr device is rather unsatisfactory, in the sense that it
    defines a deBruijn-style specification, next to the HOAS for patterns.
    Somehow, it weakens the storyline. Can we have a specification style
    in HOAS that maps to the dptrs?
    A completely deBruijn-style DSL is more uniform.
*)

end

module type InjectSymantics = sig
  include SymPat
  type 'ctx ext
  val empty_ext: 'ctx ext
  val (|++|): 'ctx ext -> 'ctx ext -> 'ctx ext

  val join: ('s,'a) ctx -> 's ext -> ('a -> ('s,'b) pat) -> 'b shape repr
end

(* TODO: make variation using Oleg's dptr example *)
module InjectionExample = struct
  open Hlists

  (* meta data*)
  type  meta = int
  let merge: meta -> meta -> meta = fun x y -> x*y
  module MCtx = HList(struct type 'a t = meta end)
  module MFold = HFOLD(MCtx)
  let merge_hlist metas =
    MFold.(fold { zero = 1; succ = fun m next -> merge m (next ()) } metas)

  type 'a repr = 'a
  type 'a shape = ('a * meta) list

  (* Input variable context *)
  type 'a el_repr = 'a repr * meta repr
  type (_,_) var = Bind: 'a shape repr -> ('a, 'a el_repr) var
  (* TODO: can't we use nested HList application to construct accumulated pointer sequences? *)
  module Ctx = HList(struct type 'a t = ('a, 'a el_repr) var end)

  (* both phantom types must be part of the gadt definition, in order to properly inspect their shapes! *)
  type (_,_) ctx' =
    | Z: (unit,unit) ctx'
    | S: ('s,'a) ctx' -> ('b * 's, 'b el_repr * 'a) ctx'
  type ('s,'a) ctx = ('s,'a) ctx' * 's Ctx.hlist
  let from: 'a shape repr -> ('a, 'a el_repr) var = fun s -> Bind s
  let cnil: (unit,unit) ctx = (Z, Ctx.nil)
  let (@.): type s s' a b. (s,a) var -> (s', b) ctx -> (s * s', a * b) ctx =
    (fun v ctx ->
      match v, ctx with
      | (Bind _), (n,ctx) -> (S n, Ctx.cons v ctx))

  (* expressions *)
  let pair: 'a repr -> 'b repr -> ('a * 'b) repr  = fun x y -> (x,y)
  let bool: bool -> bool repr = fun b -> b

  (* patterns *)
  type ('ctx, 'a) pat = 'ctx MCtx.hlist -> 'a shape
  let zero: ('ctx, 'a) pat = fun _ -> []
  let where: bool repr -> ('m, 'a) pat -> ('m, 'a) pat = fun b p -> if b then p else zero
  let yield: 'a repr -> ('m, 'a) pat = fun v ctx -> [(v, merge_hlist ctx)]

  module CExt = HList(struct type 'a t = 'a el_repr end)
  type 'ctx ext = 'ctx CExt.hlist -> unit
  let empty_ext: 'a ext = fun _ -> ()
  let (|++|) f g ctx = (f ctx); (g ctx)

  let rec cart : type s r w. (s,r) ctx -> (s CExt.hlist -> r -> s MCtx.hlist -> w list) -> w list = fun hs f ->
    match hs with
    | (Z, Ctx.Z) -> f CExt.nil () MCtx.nil
    | (S n, Ctx.S (Bind ls, hs)) ->
       List.(concat @@ map (fun el_rep ->
                           cart (n,hs)
                             (fun exs xs ms ->
                               f (CExt.cons el_rep exs) (el_rep, xs) (MCtx.cons (snd el_rep) ms))) ls)

  let join: type s a b. (s, a) ctx -> s ext -> (a -> (s,b) pat) -> b shape repr =
    fun ctx ext body -> cart ctx (fun ectx tuple -> ext ectx; body tuple)

  (* Example dptr-based extensions: *)
  module CExtP = HListP(CExt)
  let show: type i ctx. (i el_repr -> string) -> (i,ctx) ptr -> ctx ext =
    fun format ptr ctx ->
    let v_i = CExtP.proj ptr ctx in
    print_string (format v_i)
  let msg: string -> 'ctx ext = fun msg _ -> print_string msg

  let show_string () = show (fun (v,t) -> Printf.sprintf "(\"%s\",%d)" v t)
  let show_int () = show (fun (v,t) -> Printf.sprintf "(%d,%d)" v t)
  let show_float () = show (fun (v,t) -> Printf.sprintf "(%f,%d)" v t)
end

module TestInject(S: InjectSymantics) = struct
  open S
  let binders a b c =
    ((from a) @. (from b) @. (from c) @. cnil)
  let exp a b i =
    join ((from a) @. (from b) @. cnil)
      i
      (fun ((x,_), ((y,_), ())) -> (* TODO try to get curried version *)
        (where (bool true)
           (yield (pair x y))))
end

module TestInst = struct
  include TestInject(InjectionExample)
  open InjectionExample
  open Hlists
  let test () =
    let ext = (msg "extension:\n")
              |++| (show_int () (Ptrs.n0 ()))
              |++| (msg " ")
              |++| (show_string () (Ptrs.n1 ()))
              |++| (msg "\n") in
    let s = exp [(1,1);(2,3);(3,5)] [("a",2);("b",4);("c",6);("d",8)] ext in
    let s2 =
      join ((from s) @. (from [(3.1,10)]) @. cnil)
        empty_ext
        (fun ((x,_), ((y,_), ())) ->
          yield (pair x y))
    in s2
end
