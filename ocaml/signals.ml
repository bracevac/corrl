open Hlists

module type Sym = sig
  type 'a repr
  type 'a elem
  type 'a shape

  (* val where: bool repr -> 'a repr -> 'a repr *)
  val yield: 'a elem repr -> 'a shape repr
  (* TODO: this signature is strange: *)
  val pair: 'a elem repr -> 'b elem repr -> ('a * 'b) elem repr

  type 'a ctx
  type 'a var
  val from: 'a shape repr -> 'a elem repr var
  val cnil: unit ctx
  val (@.): 'a var -> 'b ctx -> ('a * 'b) ctx

  val join: 'a ctx -> ('a -> 'b shape repr) -> 'b shape repr
end

type 'a signal = {push: 'a -> unit; subscribe: (unit -> unit) -> unit; pull: unit -> 'a}

let make_signal : unit -> 'a signal = fun () ->
  let state = ref None in
  let callbacks = ref [] in
  {push = (fun v -> state := Some v; List.iter (fun cb -> cb ()) !callbacks);
   subscribe = (fun cb -> callbacks := cb :: !callbacks);
   pull = (fun () ->
     match !state with
     | None -> failwith "Signal has no value"
     | Some v -> v);}
let make_signal' v =
  let s = make_signal () in
  s.push v;
  s

(* HList of dynamically-bound channels. Dynamic binding is
   implemented via reference cells *)
module HL = HList(struct type 'a t = 'a end)
let rec to_tuple: type a. a HL.hlist -> a =
  function
  | HL.Z -> ()
  | HL.S (x,xs) -> (x, to_tuple xs)

module Signals = HList(struct type 'a t = 'a signal end)

(* Simple dynamic binding *)
type 'a dref = unit -> 'a
module JoinState = HList(struct type 'a t = 'a dref end)
let force () =
  let module M = HMAP(JoinState)(HL) in
  (fun state ->
    let values = M.map {M.f = fun dref -> dref ()} state
    in to_tuple values)


let mk_state: type a. a Signals.hlist -> a JoinState.hlist =
  fun signals ->
  let module M = HMAP(Signals)(JoinState) in
  M.map {M.f = fun s -> s.pull} signals

let react: type a b. a JoinState.hlist -> (a -> b signal) -> b signal =
  fun join_state join_pattern ->
  let tuple = force () join_state in
  join_pattern tuple

module SignalSym = struct
  type 'a repr = 'a
  type 'a elem = 'a
  type 'a shape = 'a signal

  let pair x y = (x,y)

  let yield x =
    let s = make_signal () in
    s.push x;
    s

  type _ var =
    | Bind: 'a shape repr -> 'a elem repr var
  module Ctx = HList(struct type 'a t = 'a var end)

  type 'a ctx = 'a Ctx.hlist
  let from s = Bind s
  let cnil = Ctx.nil
  let (@.) = (Ctx.cons)

  let join_impl () =
    let module M = HMAP(Ctx)(Signals) in
    let module F = HFOREACH(Signals) in
    let out_signal = make_signal () in
    (fun ctx pattern ->
      let signals = M.map {M.f = fun (Bind s) -> s } ctx in
      let join_state = mk_state signals in
      let callback () =
        let s = react join_state pattern in
        out_signal.push (s.pull ())
      in
      let _ = F.foreach {F.f = fun s -> s.subscribe (callback)} signals in
      out_signal)

  let join: type a b. a ctx -> (a -> b shape repr) -> b shape repr =
    fun ctx join_pattern -> join_impl () ctx join_pattern

end

module TestSym(S: Sym) = struct
  open S
  let exp temp_sensor smoke_sensor =
    join
      ((from temp_sensor) @. (from smoke_sensor) @. cnil)
      (fun (t, (s, ())) ->
        yield (pair t s))
end

let test () =
  let module Exp = TestSym(SignalSym) in
  let temp = make_signal' 0.0 in
  let smoke = make_signal' false in
  let joined = Exp.exp temp smoke in
  let _ = joined.subscribe (fun () ->
              let (x,y) = joined.pull () in
              Printf.printf "(%f,%b)\n" x y)
  in
  temp.push 30.0;
  smoke.push true;
  temp.push 20.0;
  smoke.push true;
  temp.push 10.0;
  temp.push 5.0;
  smoke.push false
