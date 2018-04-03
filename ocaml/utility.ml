module type SomeT = sig
  type t
end

let println s = print_string s; print_newline ()

let flatMap f l = List.concat (List.map f l)

let rec update_first p f = function
  | [] -> []
  | x::xs ->
     if (p x)
     then (f x) :: xs
     else x :: (update_first p f xs)
