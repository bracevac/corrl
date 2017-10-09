
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
end

module Async : Async = struct

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

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

(* Time models are monoids over some time representation *)
module type TimeModel = sig
  type time                
  val ( <@> ) : time -> time -> time
  val tzero : time  
end

(* An event is evidence of something that happened at a specific time *)                      
module type Event = sig
  type time
  type 'a evt     
end

module Event(T: TimeModel) = struct
  type time = T.time
  type 'a evt = Ev of 'a * time
end
                           
(* Our default time model is interval-based *)
module Interval : TimeModel = struct
  type time = int * int
  let ( <@> ) (a,b) (c,d) = (min a c, max b d)
  let tzero = (max_int, min_int) (* representation of empty interval *)                         
end
                 
module Evt = Event(Interval)

(* Event streams *)           
module Reactive(E: Event) = struct
  type 'a react = RNil | RCons of ('a E.evt) * ('a react Async.promise)
end

module R = Reactive(Evt)                                
           
open Evt
open R       

module type SomeT = sig
  type t
end     

(* TODO have separate module types *)                                               
module SingleWorld(T: SomeT) = struct
  type t = T.t
  effect Yield  : t -> unit
  effect Cancel : unit

  let yield x = perform (Yield x)
  let cancel = perform Cancel              
         
  let handler action =
    match action () with
    | x -> None  
    | effect (Yield v) _ -> Some v
    | effect Cancel _ -> None                      
end

module ManyWorlds = struct
  effect Fork : bool  

  let fork = perform Fork

  (* TODO can we have this mutability-free? Seems we need shallow handlers. *)
  let run onDone worlds action =
    let next () =
      begin match !worlds with
      | [] -> ()
      | x::xs -> worlds := xs; x ()
      end in
    match action () with
    | x -> onDone(x); next ()
    | effect Fork k ->
       let k2 = Obj.clone k in (* This is where we need multi-shot continuations  *)
       let choices = [fun () -> continue k true; fun () -> continue k2 false] in
       worlds := !worlds @ choices;
       next ()

  let handler onDone action = run onDone (ref []) action     
end

module Output(T: SomeT) = struct
  type t = T.t
  effect Out: t -> unit
end



