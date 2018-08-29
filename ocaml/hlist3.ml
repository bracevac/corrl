(* So far, challenges are:

- Composing variadic definitions. Seems one needs a big flat
  composition.

- Generating type relations/proofs after the fact, e.g. given an hlist,
  output its reversed form and proof that its type is the reversal.
*)

(* End user perspective:
  let r = correlate <n> [r1;...; rn]
    (fun [Ev (x1,t1);...;Ev (xn,tn)] ->
       if ...
       then yield (Ev (f (x1,...,xn), t1 u...u tn  ))
       else fail ()) *)


#use "topfind";;
#mod_use "utility.ml";;
#mod_use "handlers.ml";;
#mod_use "count.ml";;

open Utility
open Handlers
(* open Count *)

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

type 'a typ = Typ
let typ_int: int typ = Typ
let typ_list: 'a typ -> 'a list typ = (fun _ -> Typ)
let typ_string: string typ = Typ
let typ_float: float typ = Typ
let typ_char: char typ = Typ
let witness: 'a -> 'a typ = (fun _ -> Typ)
let hlist_shape_typ: 'a hlist typ -> 'a typ = (fun _ -> Typ)

(* Better to have a separate hlist type for types, because we can qualify
   type variables as being decomposable into typ values. *)
type _ typ_list =
  | TNil: unit typ_list
  | TCons: ('a typ * 'b typ_list) -> ('a * 'b) typ_list
let rec witness_hlist: type a. a hlist -> a typ_list =
  function
  | HNil -> TNil
  | HCons (x,xs) -> TCons (witness x, witness_hlist xs)

(* Some test values *)
let my_typ_list = TCons (typ_int, TCons (typ_string, TCons (typ_float, TNil)))
let my_hlist = HCons (1, HCons ("2", HCons (3.1, HNil)))
let my_typ_list_list = TCons (typ_list typ_int, TCons (typ_list typ_string, TCons (typ_list typ_float, TNil)))
let my_list_hlist = HCons ([1;2;3], HCons (["A";"B"], HCons ([0.3;0.2;0.1], HNil)))

module HListRev = struct
  (* TODO the advantages of having such a separation between proof
     and impl are not clear. Couldn't we just directly use the required
     functions in one big composition?

     One would be that we could generate different implementations
     from sig (e.g., staged implementations).
   *)
  module Sig = struct
    type (_,_,_) proof =
    (* The crucial trick: we supply a witness at the base that
       determines the overall result. TODO explain what goes wrong
       if we do it without it. *)
    (* We aid the type system of the host language by computing the right type of the result. *)
    (* Why have the GADT at all, if the 'a typ witness at the Base already yields the type of the reversal? This GADT enables the elementwise inspection of the type! *)
    | Base: 'a typ_list -> (unit,'a,'a) proof
    | Step: ('a typ * ('b, 'a * 'c,'d) proof) ->
            ('a * 'b, 'c, 'd) proof

    (* Consequently, the variadic version models a proof value with a "hole", the type of the hole
       changes with each extra Step layer we add. Holed proof values can be incrementally extended,
       using the numeral combinators. *)
    let z TNil = (fun typ_ctx -> Base typ_ctx)
    let s n (TCons (t,ts)) =
      let next = n ts in
      (fun typ_ctx ->
        Step (t, next (TCons (t,typ_ctx))))

    (* At some point, we must plug the hole, obtaining a closed proof value, that however cannot be
       incrementally extended any longer. *)
    let close n ts = n ts TNil (* We plug with TNil, but any other type list would work, too. The holed representation actually models a reverse + append combinator. *)

    let rec extract_typ_list: type a b c. (a,b,c) proof -> c typ_list = function
      | Base ts -> ts
      | Step (_, proof) ->
         extract_typ_list proof
  end

  let z HNil = (fun tl -> tl)
  let s n (HCons (x,xs)) =
    let next = n xs in (* to make the fold obvious *)
    (fun tl -> next (HCons (x, tl)))

  let apply n hs = n hs HNil

  let rec from_sig: type i acc o. (i,acc,o) Sig.proof -> i hlist -> acc hlist -> o hlist =
    function
    | Sig.Base _ -> z
    | Sig.Step (_, next) ->
       let n = from_sig next in
       s n
end

module type ElementWise = sig
  type 'a wrap
  (* (t,u) proof iff t and u are hlist phantom types (i.e. nested tuple types)
     with (1) same length and (2) u_i = t_i wrap, i.e., u is type constructor (_ wrap) applied to t elementwise.   *)
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

module ListWrap = ElementWiseWrap(struct type 'a wrap = 'a list end)

(*
   Nested 'flatMapping' (iteration) of n lists, which is parametric in the
   consumer (= innermost layer).

   'a1 list * ... * 'an list -> (('a1 * ... * 'an) -> 'b list) -> 'b list
 *)
module Cart = struct
  (* These are not the final numerals (and thus primed), because the fold reversed the
     the order of iteration over the lists and the consumer parameters. *)
  let z' HNil = (fun ctx consumer -> consumer ctx)
  let s' n (HCons (l, ll)) = fun ctx consumer ->
    let next = n ll in
    Lists.flatMap l (fun x -> next (HCons (x,ctx)) consumer)

  (* We compose with reversal to obtain original left-to-right order of iteration and consumer parameters *)
  let z = HCons (z', HCons (HListRev.z, HNil))
  let s (HCons (n', HCons (rn, HNil))) = HCons (s' n', HCons (HListRev.s rn, HNil))

  let apply (HCons (n', HCons (rn, HNil))) hs = n' (HListRev.apply rn hs) HNil

  let once   () = apply (s z)
  let twice  () = apply (s (s z))
  let thrice () = apply (s (s (s z)))
  let frice  () = apply (s (s (s (s z))))

  (* The signature describing the primed version. Similar to reversal, we must pass a context down that
     grows with each additional layer. Therefore, we work again with holed proof values that must be
     closed in the final step. Due to passing context downwards (and the shape of hlists), we reverse
     the context, which we have to undo. I.e., composition with reversal is necessary. *)
  module Sig' = struct
    (* This is very similar to reversal, with the difference that we recurse over
       an hlist of lists, but the context only carries the hlist of list element types,
       since we extract individual tuples from the lists. *)
    type (_,_,_) proof =
      | Base: 'c typ_list -> (unit, 'c, 'c) proof
      | Step: ('a, 'b * 'c, 'd) proof -> ('b list * 'a, 'c, 'd) proof

    let z TNil typ_ctx = Base typ_ctx
    let s n (TCons (t,ts)) =
      let next = n ts in
      (* Our index is an hlist of lists, but the context should bind only the element types,
         this is how we fix it: *)
      let list_elem_typ: 'a list typ -> 'a typ = (fun _ -> Typ) in
      let t = list_elem_typ t in
      (fun typ_ctx ->
        Step (next (TCons (t,typ_ctx))))

    let close n ts = n ts TNil
  end

  (* That was the generation of the version with wrong tuple order. Can we define a signature type
     that enables generating the version in the "right" order?
     It seems clear that we must rely on HListRev, so we would expect the signature to reflect
     that by referring to HListRev.Sig somehow:
   *)
  module Sig = struct
    type (_,_,_) proof =
      | Prod: ('a,unit,'c) HListRev.Sig.proof * ('c,unit,'e) Sig'.proof -> ('a, unit, 'e) proof

      let z () = (HListRev.Sig.z, Sig'.z)
      let s n =
        let (rn,cn) = n () in
        (fun () -> (HListRev.Sig.s rn, Sig'.s cn))

      let close (rn,cn) ts =
        let rev_proof = HListRev.Sig.close rn ts in
        (* To ensure the relations as demanded by the proof type definition, we need computations on the level of typ and typ lists. In this case, we can just extract the reversed type list from the proof. *)
        let ts_rev = HListRev.Sig.extract_typ_list rev_proof in
        Prod (rev_proof, Sig'.close cn ts_rev)

      (* Fun fact: this commented-out construction accepts only "palindrome" hlists  *)
      (* let close (rn,cn) ts = Prod (HListRev.Sig.close rn ts, Sig.close cn ts) *)
  end

  let rec synth': type a b c d. (a,b,c) Sig'.proof -> a hlist -> b hlist -> (c hlist -> d list) -> d list = function
    | Sig'.Base _ -> z'
    | Sig'.Step next -> s' (synth' next)

  (* Disadvantage: information hiding not really possible, i.e., clients of this module must be aware of the primed version, which should be considered an implementation detail. *)
  let synth: type a b c. (a,unit,b) Sig.proof -> a hlist -> (b hlist -> c list) -> c list = function
    | Sig.Prod (revp, cartp) ->
       let rev = HListRev.from_sig revp in
       let cart' = synth' cartp in
       (fun hs -> cart' (rev hs HNil) HNil)

  (* TODO: now give staged versions of all the combinators, how far can we eliminate overhead, e.g., from
     composition?  *)

  let test () =
    let three = Sig.(s (s (s z))) in
    let proof = Sig.close (three ()) in
    let cart = synth (proof my_typ_list_list) in
    cart my_list_hlist (fun x -> [x])
end

module Unzip = struct
  let z HNil = (HNil, HNil)
  let s n (HCons ((x,y),hs)) =
    let (xs,ys) = n hs
    in (HCons (x,xs), HCons (y,ys))

  let apply n hs = n hs

  let once   () = apply (s z)
  let twice  () = apply (s (s z))
  let thrice () = apply (s (s (s z)))
  let frice  () = apply (s (s (s (s z))))
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
let evt_typ  : 'a typ -> 'a evt typ  = (fun _ -> Typ)
let tparam_of: 'a r typ -> 'a typ    = (fun _ -> Typ)
let list_typ : 'a typ -> 'a list typ = (fun _ -> Typ)
let slot_typ : 'a typ -> 'a slot typ = (fun _ -> Typ)

(* Idea: seems we could replace all the GADTs with final tagless encodings *)
module WrapId = ElementWiseWrap(struct type 'a wrap = 'a end)
module WrapEvtSlot = ElementWiseWrap(struct type 'a wrap = 'a evt slot end)
module WrapEvt = ElementWiseWrap(struct type 'a wrap = 'a evt end)
module WrapTyp = ElementWiseWrap(struct type 'a wrap = 'a typ end)

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

(* Can we encode fixed hlists to proper n-tuples, avoiding allocation overhead?  *)

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
  let rec mkSlots: type i s. (i,s) WrapEvtSlot.proof -> s hlist =
    let evt_from_slot: type a. (module SLOT with type t = a evt) typ -> a evt typ = (fun _ -> Typ) in
    function
    | WrapEvtSlot.Base -> HNil
    | WrapEvtSlot.Step (_, evidence, rest) ->
       HCons ((mkSlot (evt_from_slot evidence)), (mkSlots rest))

  (* Forget the element types of a slot hlist *)
  let rec existentialize: type i s. (i,s) WrapEvtSlot.proof -> s hlist -> (module SLOT) list =
    function
    | WrapEvtSlot.Base -> (fun HNil -> [])
    | WrapEvtSlot.Step (_, _, rest) ->
       let ex_tl = existentialize rest in
       (fun (HCons (s, hs)) -> (Obj.magic s) :: (ex_tl hs))
end

(* The next step: binding the correct lists to the cartesian impl.
   I.e., at each position i, generate the function
   cart_i =
     fun (p: a_i evt * Count.t ref) ->
       let mboxes = (mailbox s1, ..., mailbox s(i-1), [p], mailbox s(i+1), ..., mailbox sn) in
       cartesian n mboxes

   Problem is: this requires a position-dependent processing of hlists

   Options:

   1. Do it at the level of homogenous slot list with abstract types and
   make a cast. Seems relatively easy but not nice.

   2. Generate another polyvariadic definition for computing all the zippers
   of an hlist, then transform each zipper in that list to the functions above.

   3.

 *)

(*
   By now, we have a good grasp of how to fold producing context-dependent definitions.
   We could view the binding problem as considering each slot in the slot list
   in relation to its neighborhood. That is, a zipper.
 *)
module Zipper = struct
  module Sig = struct
    type (_,_,_) proof =
      | Base: 'a typ_list -> (unit,'a,unit) proof
      | Step: ('a, 'b * 'c, 'd) proof ->
              ('b * 'a, 'c, ('c hlist * 'b * 'a hlist) * 'd) proof

    let z TNil tctx = Base tctx
    let s n (TCons (t,ts)) tctx =
      let next = n ts (TCons (t,tctx)) in
      Step next

    let close n ts = n ts TNil
  end

  let z HNil ctx = HNil
  let s n (HCons (x,xs)) ctx =
    let tail = n xs (HCons (x,ctx)) in
    HCons ((ctx,x,xs), tail)

  (* TODO: rename to close everywhere *)
  let apply n hs = n hs HNil

  let rec synth: type a b c. (a,b,c) Sig.proof -> a hlist -> b hlist -> c hlist =
    function
    | Sig.Base _ -> z
    | Sig.Step next -> s (synth next)
end

(* Now we need to transform each zipper of a slot hlist to the corresponding
   invocation of cartesian.

   ('a hlist, 'b, 'c hlist) -> 'b event -> rev a ++ b ++ c

   Indeed, the hole representation of reversal is quite useful,
   since it actually models reverse + append

*)





(*
  Other construction that works with abstract slot list (I think):

  turn the abstract slot list into thunks that retrieve the inboxes,
  or at the ith position invoke an effect. A handler transforms this
  into a function that continues.

  Problem: linking this with the trigger effect, seems to require a cast...
*)


module JoinsImpl(T: JOIN) = struct
  module Sig = T
  effect Trigger: T.joined hlist -> unit
  let trigger v = perform (Trigger v)

  let slots: T.slots hlist = Gen.mkSlots T.p_index_slot_rel
  let slots_u: (module SLOT) list = Gen.existentialize T.p_index_slot_rel slots

  let forAll () = gen_handlers slots_u (fun i (s: (module SLOT)) ->
                   let module S = (val s) in
                   (fun action ->
                     try action () with
                     | effect (S.Push x) k ->
                        S.(setMail ((x,(ref Count.Inf)) :: (getMail ())));
                        continue k (S.push x)))



  (* module Cartesian = struct
   *   let gc () = failwith "not implemented"
   *   let reify = gen_handlers slots_u (fun i (s: (module SLOT)) ->
   *                   let module S = (val s) in
   *                   (\* TODO can we use MetaOCaml's first-class pattern matching to
   *                      eliminate the overhead from thunks? In the end, we really
   *                      want to fuse all the cases into one "big" handler. *\)
   *                   (\* let cartesian_i = TODO in *\)
   *                   (fun action ->
   *                     try action () with
   *                     | effect (S.Push v) k ->
   *                        let x = List.find (fun (ev,_) -> ev = v) (S.getMail ()) in
   *                         (\* forkEach trigger (cartesian0 x); TODO *\)
   *                        gc ();
   *                        continue k ()))
   *
   *   type slot_zipper = ((module SLOT) list * (module SLOT) * (module SLOT) list)
   *   let slots_zippers = Lists.zippers slots_u
   *
   *   module WrapCount = ElementWiseWrap(struct type 'a wrap = 'a * (Count.t ref) end)
   *   module WrapList = ElementWiseWrap(struct type 'a wrap = 'a list end)
   *
   *   (\* unzip events paired with their life time counters. TODO: functions could be eliminated with staging  *\)
   *   let rec gen_unzip: type j j'. (j, j') WrapCount.proof -> j' hlist -> (j hlist * ((Count.t ref) list)) =
   *     function
   *     | WrapCount.Base ->
   *        (function
   *         | HNil -> (HNil, []))
   *     | WrapCount.Step (_,_,rest) ->
   *        let ftail = gen_unzip rest in
   *        (function
   *         | HCons ((evt,c), hs) ->
   *            let (etail, ctail) = ftail hs in
   *            ((HCons (evt, etail)), (c :: ctail)))
   *
   *   (\* Encodes the recursion scheme of the nested cartesian product calculation (discussion at bottom) *\)
   *   module CartesianRec = struct
   *     type ('res,_,_) proof = (\* TODO factor out res *\)
   *       | Base:  ('res, unit, 'res) proof
   *       | Step:  ('res, 'b, 'a * 'c) proof -> ('res, 'a * 'b, 'c) proof
   *    end
   *
   *   let gen_cartesian: type index joined joined' rjoined'. (\* TODO the params seem redundant, since we can access SIG *\)
   *                           int ->
   *                           (index, joined) WrapEvt.proof ->
   *                           (joined,joined') WrapCount.proof ->
   *                           (joined',rjoined') HListRev.proof ->
   *                           (joined',rjoined', unit) CartesianRec.proof ->
   *                           unit = (\*TODO FIX*\)
   *     (fun i proof proof' rproof cart_proof ->
   *       let unzip = gen_unzip proof' in
   *       (\* This function is the innermost layer of the nested cartesian product. It yields an admissible tuple,
   *          if all components have non-zero life times:
   *          (t1 evt * Count.t ref) * ... * (tn evt * Count.t ref) -> (t1 evt * ... * tn evt) list *\)
   *       let yield: joined' hlist -> (joined hlist) list = (fun tuple ->
   *           let (events, lives) = unzip tuple in
   *           if (List.for_all (fun r -> Count.lt_i 0 !r) lives)
   *           then
   *             begin
   *               List.iter (fun r -> r := Count.dec !r) lives;
   *               [events]
   *             end
   *           else [])
   *       in
   *
   *       (\* Step 1: cartesian product accepting n lists, note that lists must be supplied in reverse order n..1 *\)
   *       let rec cart: type a al ctx. (joined',a,ctx) CartesianRec.proof ->
   *                     (a,al) WrapList.proof ->
   *                     al hlist ->
   *                     ctx hlist -> (joined hlist) list =
   *         (fun cart_proof wrap_proof mailboxes ->
   *           match cart_proof, wrap_proof, mailboxes with
   *           | CartesianRec.Base, WrapList.Base, HNil ->
   *              (yield)
   *           | CartesianRec.(Step cnext), WrapList.(Step (_,_,wnext)), HCons (mbox, ms) ->
   *              let next = cart cnext wnext ms in
   *              (fun ctx ->
   *                Lists.flatMap mbox (fun x -> next (HCons (x, ctx)))))
   *       in
   *       (\* Step 2: generate the function that given a new event from the ith input, obtains all the
   *          mailboxes to feed to the cart implementation (in *reverse* order n .. 1)):
   *
   *          fun p: (ti evt * Count.t ref) ->
   *              (mailbox Sn, ..., mailbox S(i+1), [p], mailbox S(i-1), ..., mailbox S1 ) *\)
   *       (\*  We need to distinguish the ith slot from the others, hence the zipper.
   *           Overall, we generate a function that, given the ith event and its life counter,
   *           Computes nested flatMaps (=cartesian product) over the other slots's memory lists:
   *           (ti evt * Count.t) -> (t1 evt * ... * tn evt) list
   *          *\)
   *       let zipper = List.nth slots_zippers i in
   *       let callback i = ()
   *       in () (\* TODO should intuitively be callback i (gen i) *\)
   *
   *     ) *)




end

(* next step: implement cartesius core handlers *)
(* DSL *)
(* extensions *)
