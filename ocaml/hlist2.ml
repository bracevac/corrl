#use "topfind";;
#mod_use "utility.ml";;
#mod_use "handlers.ml";;
#mod_use "count.ml";;

open Utility
open Handlers
(* open Count *)

(* Simple heterogeneous list GADT *)
(* TODO overload standard list syntax *)

module HList = struct
  type _ hlist =
    | HNil : unit hlist
    | HCons : ('a * 'b hlist) -> ('a * 'b) hlist

  let rec length: type a. a hlist -> int = function
    | HNil -> 0
    | HCons (_, hs) -> 1 + (length hs)
end

open HList

(* This can be generalized to a general accumulator-style recursion scheme functor *)
module HListRev = struct
  type (_,_,_) aux =
    | ABase: (unit, 'acc, 'acc) aux
    | AStep: ('b, 'a * 'acc, 'r) aux -> ('a * 'b,'acc, 'r) aux

  type (_,_) proof =
    | Proof: ('a, unit, 'b) aux -> ('a, 'b) proof

  (* General pattern: we generate functions from proof GADTs. Proof values do not occur in the
     generated function. That is, we have a clear separation of stages between DSL type checking/code generation
     and actual run time.  *)
  let rec gen_rev_aux: type a b c. (a, b, c) aux -> b hlist -> a hlist -> c hlist =
    function
    | ABase -> (fun acc _ -> acc)
    | AStep n ->
       let next = gen_rev_aux n in
       fun acc (HCons (x, xs)) -> next (HCons (x, acc)) xs

  (* Generate reversal function on hlists *)
  (* TODO: the generated function is *monomorphic*, can we obtain a polymorphic version using records?  *)
  let gen_rev = function
    | Proof aux -> gen_rev_aux aux HNil
end


(* emulate the type constructors in our framework *)
type 'a r = 'a list
type 'a evt = 'a

(* a slot exposing a generative effect, just as in our framework *)
module type SLOT = sig
  type t
  effect Push: t -> unit
  val push: t -> unit
  effect GetMail: (t * (Count.t ref)) list
  val getMail: unit -> (t * (Count.t ref)) list
  effect SetMail: (t * (Count.t ref)) list -> unit
  val setMail: (t * (Count.t ref)) list -> unit
  val memory: (t * (Count.t ref)) list
end
type 'a slot = (module SLOT with type t = 'a)
type slot_ex = (module SLOT)

(* to construct type witnesses and map types to other types *)
type 'a typ = Typ
let witness  : 'a -> 'a typ          = (fun _ -> Typ)
let evt_typ  : 'a typ -> 'a evt typ  = (fun _ -> Typ)
let tparam_of: 'a r typ -> 'a typ    = (fun _ -> Typ)
let list_typ : 'a typ -> 'a list typ = (fun _ -> Typ)
let slot_typ : 'a typ -> 'a slot typ = (fun _ -> Typ)

module type ElementWise = sig
  type 'a wrap
  (* (t,u) proof iff t and u are hlist phantom types (i.e. nested tuple types)
     with (1) same length and (2) u_i = t_i wrao, i.e., u is type constructor (_ wrap) applied to t elementwise.   *)
  type (_, _) proof =
    | Base: (unit, unit) proof
    | Step: ('a typ * 'a wrap typ * ('b, 'c) proof) -> (('a * 'b), ('a wrap * 'c)) proof
end

module ElementWiseWrap(T: sig type 'a wrap end): (ElementWise with type 'a wrap = 'a T.wrap) =
  struct
    type 'a wrap = 'a T.wrap

    type (_, _) proof =
      | Base: (unit, unit) proof
      | Step: ('a typ * 'a wrap typ * ('b, 'c) proof) -> (('a * 'b), ('a wrap * 'c)) proof
  end

(* Idea: seems we could replace all the GADTs with final tagless encodings *)
module WrapEvtSlot = ElementWiseWrap(struct type 'a wrap = 'a evt slot end)
module WrapEvt = ElementWiseWrap(struct type 'a wrap = 'a evt end)
module WrapTyp = ElementWiseWrap(struct type 'a wrap = 'a typ end)

(* The gist: we polyvariadically create n-way JOIN modules, which bundle
   the required type signatures and carry *no* implementation code.
   Actual implementation code is *oblivious* to polyvariadicity, working
   on uniformly typed lists/arrays of slot modules. JOIN modules are used
   to recover types (via Obj.magic) when necessary. Lame, but it does the job. *)

(* End user perspective:
  let r = correlate <n> [r1;...; rn]
    (fun [Ev (x1,t1);...;Ev (xn,tn)] ->
       if ...
       then yield (Ev (f (x1,...,xn), t1 u...u tn  ))
       else fail ()) *)


(* A join consists of a type 'index' (hlist phantom type describing the arity and types) and
   a hlist of phantom type 'slots', which is related to 'index' by the Hlist_Match_Slot_Evt relation. *)
module type JOIN = sig
  type index  (*  a1 ... an    *)
  type joined (*  a1 evt ... an evt *)
  type slots  (*  a1 evt slot ... an evt slot *)

  (* You can construct a JOIN only if you can prove that the types are related in all of the following ways:  *)
  val p_index_joined_rel: (index, joined) WrapEvt.proof
  val p_index_slot_rel: (index, slots) WrapEvtSlot.proof
end
type ('a, 'b, 'c) join = (module JOIN with type index = 'a
                                       and type joined = 'b
                                       and type slots = 'c)

(* Less clunky JOIN constructor function *)
let mkJoin (type i j s)
      (p_j: (i,j) WrapEvt.proof)
      (p_s: (i,s) WrapEvtSlot.proof) =
  (module struct
     type index = i
     type joined = j
     type slots = s
     let p_index_joined_rel = p_j
     let p_index_slot_rel = p_s
   end: (JOIN with type index = i and type joined = j and type slots = s))


(* Polyvariadic join construction, an application of the techniques in http://okmij.org/ftp/Computation/extra-polymorphism.html#poly-var,
   and Daniel Fridlender and Mia Indrika: Do We Need Dependent Types? J. Functional Programming, 2000. *)
module PolyJoin = struct (* TODO should we have an uncurried variant? *)
  let z k = k (mkJoin WrapEvt.Base WrapEvtSlot.Base)
  let s (type a b c) n k element_typ =
    n (fun (v: (a, b, c) join) ->
        let module J = (val v) in
        let event_typ = evt_typ element_typ in
        let slot_typ = slot_typ event_typ in
        let proof_joined = WrapEvt.Step (element_typ, event_typ, J.p_index_joined_rel) in
        let proof_slots = WrapEvtSlot.Step (element_typ, slot_typ, J.p_index_slot_rel) in
        let j = mkJoin proof_joined proof_slots in
        k j)

  let mk n = n (fun x -> x)

  (* tests *)
  let zero   = mk z
  let once   = mk (s z) (witness "1")
  let twice  = mk (s (s z)) (witness "1") (witness 2)
  let thrice = mk (s (s (s z))) (witness "1") (witness 2) (witness 3.0)

(*
 let bad = once (witness 1) (witness 2);;
           ^^^^ *)
end

module Lists = struct
  let rec take n l =
    match n, l with
    | i, x :: xs when i > 0 -> x :: (take (i - 1) xs)
    | _, _ -> []

  let rec drop n l =
    match n, l with
    | i, x :: xs when i > 0 -> drop (i - 1) xs
    | _, _ -> l

  let flatMap l f = List.concat (List.map f l)

  let zipper n l = (take n l, List.nth l n, drop (n+1) l)
  let zippers l = List.mapi (fun i _ -> zipper i l) l
  let rev_zipper (l,c,r) = (List.rev r, c, List.rev l)
  type 'a zipper = 'a list * 'a * 'a list
end

module Gen = struct
  let mkSlot (type s) (t: s typ) =
    (module struct
       type t = s
       effect Push: t -> unit
       let push v = perform (Push v)
       effect GetMail: (t * (Count.t ref)) list
       let getMail () = perform GetMail
       effect SetMail: (t * (Count.t ref)) list -> unit
       let setMail v = perform (SetMail v)
       let memory = []
     end: (SLOT with type t = s))

  (* Generate slot binding from proofs *)
  let rec mkSlots: type i s. (i,s) WrapEvtSlot.proof -> (module SLOT) list =
    (function
     | WrapEvtSlot.Base -> []
     | WrapEvtSlot.Step (_, evidence, rest) ->
        (* Surprisingly, we have to cast here *)
        (Obj.magic (mkSlot evidence)) :: (mkSlots rest))
end

module JoinsImpl(T: JOIN) = struct
  module Sig = T
  effect Trigger: T.joined hlist -> unit
  let trigger v = perform (Trigger v)

  let slots: (module SLOT) list = Gen.mkSlots T.p_index_slot_rel

  let forAll = gen_handlers slots (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        S.(setMail ((x,(ref Count.Inf)) :: (getMail ())));
                        continue k (S.push x)))

  module Cartesian = struct
    let gc () = failwith "not implemented"
    let reify = gen_handlers slots (fun i (s: (module SLOT)) ->
                    let module S = (val s) in
                    (* TODO can we use MetaOCaml's first-class pattern matching to
                       eliminate the overhead from thunks? In the end, we really
                       want to fuse all the cases into one "big" handler. *)
                    (* let cartesian_i = TODO in *)
                    (fun action ->
                      try action () with
                      | effect (S.Push v) k ->
                         let x = List.find (fun (ev,_) -> ev = v) (S.getMail ()) in
                         (* forkEach trigger (cartesian0 x); TODO *)
                         gc ();
                         continue k ()))

    type slot_zipper = ((module SLOT) list * (module SLOT) * (module SLOT) list)
    let slots_zippers = Lists.zippers slots

    module type Exists = sig
      type t
      val value: t
    end

    module WrapCount = ElementWiseWrap(struct type 'a wrap = 'a * (Count.t ref) end)
    module WrapList = ElementWiseWrap(struct type 'a wrap = 'a list end)

    (* Extract life time counters from hlist of ('a * Count.t) pairs. TODO: functions could be eliminated with staging  *)
    let rec gen_lives_list: type j j'. (j, j') WrapCount.proof -> j' hlist -> (Count.t ref) list =
      fun proof ->
        match proof with
        | WrapCount.Base ->
           (function
            | HNil -> [])
        | WrapCount.Step (_,_,rest) ->
           let ftail = gen_lives_list rest in
           (function
            | HCons ((_,c), hs) -> c :: (ftail hs))

    let rec gen_unwrap: type j j'. (j, j') WrapCount.proof -> j' hlist -> j hlist =
      fun proof ->
        match proof with
        | WrapCount.Base ->
           (function
            | HNil -> HNil)
        | WrapCount.Step (_,_,rest) ->
           let ftail = gen_unwrap rest in
           (function
            | HCons((evt,_), hs) -> HCons (evt, ftail hs))

    module CartesianRec = struct
      type ('res,_,_) proof = (* TODO factor out res *)
        | Base:  ('res, unit, 'res) proof
        | Step:  ('res, 'b, 'a * 'c) proof -> ('res, 'a * 'b, 'c) proof
     end


    let gen_cartesian: type index joined joined' rjoined'. (* TODO the params seem redundant, since we can access SIG *)
                            int ->
                            (index, joined) WrapEvt.proof ->
                            (joined,joined') WrapCount.proof ->
                            (joined',rjoined') HListRev.proof ->
                            (joined',rjoined', unit) CartesianRec.proof ->
                            unit = (*TODO FIX*)
      (fun i proof proof' rproof cart_proof ->
        (*  We need to distinguish the ith slot from the others, hence the zipper.
            Overall, we generate a function that, given the ith event and its life counter,
            Computes nested flatMaps (=cartesian product) over the other slots's memory lists:
            (ti evt * Count.t) -> (t1 evt * ... * tn evt) list
           *)
        let zipper = List.nth slots_zippers i in
        let lives_list = gen_lives_list proof' in
        let unwrap = gen_unwrap proof' in
        (* This function is the innermost layer of the nested cartesian product. It yields an admissible tuple,
           if all compontents have non-zero life times:
           (t1 evt * Count.t ref) * ... * (tn evt * Count.t ref) -> (t1 evt * ... * tn evt) list
         *)
        let yield: joined' hlist -> (joined hlist) list = (fun tuple ->
            let lives = lives_list tuple in
            let events = unwrap tuple in
            if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
            then
              begin
                List.iter (fun r -> r := Count.dec !r) lives;
                [events]
              end
            else [])
        in

        let flatMap' l f = flatMap f l in

        let rec gen: type a al ctx. (joined',a,ctx) CartesianRec.proof ->
                      (a,al) WrapList.proof ->
                      al hlist ->
                      ctx hlist -> (joined hlist) list =
          (fun cart_proof wrap_proof mailboxes ->
            match cart_proof, wrap_proof, mailboxes with
            | CartesianRec.Base, WrapList.Base, HNil ->
               (yield)
            | CartesianRec.(Step cnext), WrapList.(Step (_,_,wnext)), HCons (mbox, ms) ->
               let next = gen cnext wnext ms in
               (fun ctx ->
                 Lists.flatMap mbox (fun x -> next (HCons (x, ctx)))))
        in ()  )


    (*
       We recurse on reverse of joined':

       Base: (unit, joined', joined' -> (joined hlist) list)
       Step: (b, a * c, a * c -> (joined hlist) list) -> (a * b, c, c -> (joined hlist) list)

       In our particular case, we do not need to have the third parameter,
       since it can be recovered from the second. I.e., at each level of the
       recursion scheme, the return type is derived from the current-level
       context/accumulator:

       Base: (unit, joined')
       Step: (b, a * c) -> (a * b, c)

       Return type is c -> (joined hlist) list

       However, this does not have to be the case in general. For example, in
       HListRev, we need to propagate the result from the bottom up.
       It seems the general scheme is

       Base:  (unit, a, a bot)
       Step:  (b, (a, c) acc, r) -> (a * b, c, (c , r) res)

       where bot: * -> *, acc: * x * -> *, res: * x * -> *.

       In the case of list reversion, the instance would be:
            a bot := a,
       (a, c) acc := a * c,
       (c, r) res := r

       For the cartesian product, we would have the instance
            a bot := a -> (joined hlist) list,
       (a, c) acc := a * c,
       (c, r) res := c bot

       It seems possible (and interesting) to encode all of this
       in final tagless.

       Returning back to the n-way cartesian product:

       Untyped pseudo code (recurse on the reversed list of inboxes)

       Nil -> (yield)     (* As defined above *)
       mbox :: ms ->
       let f = recurse ms
       (fun ctx ->
          flatMap mbox (fun x -> f :: ctx)))

       Signature:

       type a ctx. (a, ctx) proof -> a hlist -> (ctx hlist -> (joined hlist) list) *)


  end


end

(* next step: cartesian impl *)
(* DSL *)
(* extensions *)
