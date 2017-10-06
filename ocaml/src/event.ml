
(* Time models are monoids over some time representation *)
module type TimeModel = sig
  type time                
  val ( <@> ) : time -> time -> time
  val tzero : time  
end

(* An event is evidence of something that happened at a specific time *)                      
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
