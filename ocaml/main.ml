
module type Async = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
  val liftPromise : 'a -> 'a promise
end

module Async : Async = struct

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a
          
  type 'a promise = 'a _promise ref

  let liftPromise x = ref (Done x)                      
                      
  effect Async : (unit -> 'a) -> 'a promise
  let async f = perform (Async f)

  effect Yield : unit
  let yield () = perform Yield

  effect Await : 'a promise -> 'a
  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match main () with
        | v -> begin match !pr with
               | Waiting l -> pr := Done v; List.iter (fun task -> enqueue (fun () -> continue task v)) l
               | Done _ -> failwith "Promise already resolved"
               end;
             dequeue ()
                            
        | effect (Async f) k ->
           let p = ref (Waiting []) in           
           enqueue (fun () -> continue k p);
           fork p f

        | effect Yield k ->
            enqueue (continue k);
            dequeue ()
        | effect (Await p) k ->
            begin match !p with
            | Done v -> continue k v
            | Waiting l ->
               p := Waiting (l @ [k]);
               dequeue () 
            end
    in    
    fork (ref (Waiting [])) main
end 

                     
(* (\* Time models are monoids over some time representation *\)
 * module type TimeModel = sig
 *   type time                
 *   val ( <@> ) : time -> time -> time
 *   val tzero : time  
 * end
 * 
 * (\* An event is evidence of something that happened at a specific time *\)                      
 * module type Event = sig
 *   type time
 *   type 'a evt     
 * end
 * 
 * module Event(T: TimeModel) = struct
 *   type time = T.time
 *   type 'a evt = Ev of 'a * time
 * end
 *                            
 * (\* Our default time model is interval-based *\)
 * module Interval : TimeModel = struct
 *   type time = int * int
 *   let ( <@> ) (a,b) (c,d) = (min a c, max b d)
 *   let tzero = (max_int, min_int) (\* representation of empty interval *\)                         
 * end *)
                 
module Evt = struct
  let ( <@> ) (a,b) (c,d) = (min a c, max b d)
  let tzero = (max_int, min_int) (* representation of empty interval *)               
  type 'a evt = Ev of 'a * (int * int)                      
end

(* Event streams *)           
module Reactive = struct
  type 'a react = RNil | RCons of ('a Evt.evt) * ('a react Async.promise)

  let rec toReact: 'a Evt.evt list -> 'a react =
    function
    | [] -> RNil
    | x::xs -> RCons (x, (Async.liftPromise (toReact xs)))
end

open Evt
open Reactive       

module type SomeT = sig
  type t
end     

(* TODO have separate module types *)                                               
module SingleWorld(T: SomeT) = struct
  type t = T.t
  effect Yield  : t -> unit
  effect Cancel : unit

  let yield x = perform (Yield x)
  let cancel () = perform Cancel              
         
  let handler action =
    match action () with
    | x -> None  
    | effect (Yield v) _ -> Some v
    | effect Cancel _ -> None                      
end

module ManyWorlds = struct
  effect Fork : bool  
  let fork () = perform Fork

  (* TODO can we have this mutability-free? Seems we need shallow handlers. *)
  let run onDone worlds action =
    let next () =
      begin match !worlds with
      | [] -> ()
      | x::xs -> worlds := xs; x ()
      end in
    begin match action () with
    | x -> onDone(x); next () (* TODO should this be interleaved? *)
    | effect Fork k ->
       let k2 = Obj.clone_continuation k in (* This is where we need multi-shot continuations  *)
       let choices = [(fun () -> continue k true); (fun () -> continue k2 false)] in
       worlds := !worlds @ choices; (* Swap for DFS  *)
       next ()
    end

  let handler onDone action = run onDone (ref []) action     
end

module Output(T: SomeT) = struct
  type t = T.t
  effect Out: t -> unit
end

module type Callback = sig
  type elem
  val push : elem -> unit     
end
                     
module Callback(T: SomeT): Callback = struct
  type elem = T.t
  effect Push: elem -> unit
  let push v = perform (Push v)       
end

module State(T: SomeT) = struct
  type state = T.t
  effect Get: state
  effect Set: state -> unit
  let get () = perform Get
  let set v = perform (Set v)
                        
  let handler (init: state) action =
    let value = ref init in (* TODO try using purely functional state *)
    try action () with
    | effect Get k -> continue k !value
    | effect (Set v) k -> value := v; continue k ()                
end
                                    
let testStreams = begin
  let e1 = Ev (0, (1,1)) in
  let e2 = Ev (2, (2,2)) in 
  let e3 = Ev (4, (3,3)) in 
  let e4 = Ev (6, (4,4)) in 
  let s1 = toReact([e1; e2; e3; e4]) in

  let e5 = Ev ("1", (5,5)) in
  let e6 = Ev ("3", (6,6)) in
  let e7 = Ev ("5", (7,7)) in
  let e8 = Ev ("7", (8,8)) in
  let s2 = toReact([e5; e6; e7; e8]) in
  (s1,s2)
  end

                    
let globalContext show action = ManyWorlds.handler show action

(*TODO do we really need SingleWorld at all?*)                              
let correlate (type a) (pattern: unit -> a) =
  let module S = SingleWorld(struct type t = a end) in
  S.handler pattern

(* let forAll i action =
 *   try action () with
 *   | effect i.Push x k ->
 *      i.setMail ((initState x) :: i.getMail);
 *      continue k (i.push x)   *)

    
