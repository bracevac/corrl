module type BaseTypes = sig
  type 'a repr
  type 'a shape
  type 'a elem
  type meta
  (* Variables in patterns destructure elements into the payload and meta data *)
  type 'a el_pat = 'a repr * meta repr
end

module type JoinSym = sig
  include BaseTypes

  type ('s,'a) ctx
  type ('s,'a) var
  val from: 'a shape repr -> ('a,'a el_pat) var
  val cnil: (unit, unit) ctx
  val (@.): ('u, 'a) var -> ('s,'b) ctx -> (('u * 's), ('a * 'b)) ctx

  (* Relationship between element representation and pattern variables *)
  val elem: 'a el_pat -> 'a elem repr
  val el_pat: 'a elem repr -> 'a el_pat

  type ('ctx,'a) pat (* A pattern with result 'a el_pat and implicit metadata context 'ctx *)
  (* it seems even better to abstract the type via pat. this way, we can choose
    whether patterns are element-level or collection-level specifications:
    ('a, 'ctx) pat = 'a el_pat
    ('a, 'ctx) pat = 'a shape repr  *)

  val pair: 'a repr -> 'b repr -> ('a * 'b) repr
  val bool: bool -> bool repr

  val where: bool repr -> ('m,'a) pat -> ('m,'a) pat
  val yield: 'a repr -> ('m,'a) pat

  val join: ('s,'a) ctx -> ('a -> ('s,'b) pat) -> 'b shape repr
end

(* This language defines a join variant that permits implementation-specific extensions, captured as context-dependent monoids *)
module type JoinExtSym = sig
  include JoinSym
  (* Context-dependent extension *)
  type 'ctx ext
  val empty_ext: 'ctx ext
  val (|++|): 'ctx ext -> 'ctx ext -> 'ctx ext

  val join: ('s,'a) ctx -> 's ext -> ('a -> ('s,'b) pat) -> 'b shape repr
end

(* Common representation of the variable context using hlists. *)
module StdContextRepr(T: BaseTypes) = struct
  open T
  open Hlists
  type (_,_) var = Bind: 'a shape repr -> ('a, 'a el_pat) var
  module Ctx = HList(struct type 'a t = ('a, 'a el_pat) var end)

  (* both phantom types must be part of the gadt definition, in order to properly inspect their shapes! *)
  type (_,_) ctx' =
    | Z: (unit,unit) ctx'
    | S: ('s,'a) ctx' -> ('b * 's, 'b el_pat * 'a) ctx'
  type ('s,'a) ctx = ('s,'a) ctx' * 's Ctx.hlist
  let from: 'a shape repr -> ('a, 'a el_pat) var = fun s -> Bind s
  let cnil: (unit,unit) ctx = (Z, Ctx.nil)
  let (@.): type s s' a b. (s,a) var -> (s', b) ctx -> (s * s', a * b) ctx =
    (fun v ctx ->
      match v, ctx with
      | (Bind _), (n,ctx) -> (S n, Ctx.cons v ctx))
end
