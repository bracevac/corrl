(* Experiments and musings about Fridlender & Indrika numerals.
   Can we achieve a uniform representation, despite their claim?
   If not, where is the point where we get stuck (they don't really say)?    *)

type z = Z : z
type 'n s = S : 'n -> 'n s

module type NUM_ = sig
  type 'a zero
  type 'a succ

  type _ num =
    | Zero: 'a -> ('a zero) num
    | Succ: 'a num -> ('a succ) num


  val z: 'a -> 'a zero


end

(* Church numeral algebra *)
module type NUM = sig
  type ('a, 'b) num
  val z: 'a -> 'b -> ('a, z) num
    (* I think what we really want to express is a type-level relation between 'a and the 'c -> 'd types... *)
  val s: ('a, 'n) num -> 'b -> ('c -> 'd) -> ('d, 'n s) num
end

module Church = struct
  type ('a, 'b) num = 'a

  (* We need something richer than church numerals *)
  let zero z s = z
  let succ n z s = s (n z s)
end
