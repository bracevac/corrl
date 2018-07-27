(** Models a type for lifetime counters, which is either a finite int value or infinity. *)
(* TODO prohibit negative values *)
type t = Inf | Fin of int
let map f = function
  | Inf -> Inf
  | Fin i -> Fin (f i)
let flatMap f = function
  | Inf -> Inf
  | Fin i -> (f i)
let inc n = map (fun i -> i + 1) n
let inc_by num n = map (fun i -> i + num) n
let dec n = map (fun i -> i - 1) n
let dec_by num n = map (fun i -> i - num) n
let add n m = flatMap
                (fun i ->
                  map (fun j -> i + j) m) n
let sub n m = flatMap
                (fun i ->
                  map (fun j -> i - j) m) n
let lt n m = match (n,m) with
  | (_, Inf) -> true
  | (Fin i, Fin j) -> i < j
  | _ -> false
let lte n m = (n = m) || (lt n m)
let gt n m = not (lte n m)
let gte n m = (n = m) || (gt n m)
let lt_i i n = lt (Fin i) n
let lte_i i n = lte (Fin i) n
let gt_i i n = gt (Fin i) n
let gte_i i n = gte (Fin i) n
let inc_snd n l = List.map (fun (y,c) -> (y, (add c n))) l
let dec_snd n l = List.map (fun (y,c) -> (y, (sub c n))) l
