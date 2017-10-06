(* MIT License
 * 
 * Copyright (c) 2017 Daniel Hillerström
 * Copyright (c) 2017 Oliver Bracevac
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)


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
 
