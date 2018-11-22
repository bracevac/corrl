let println = print_endline

let rec update_first p f = function
  | [] -> []
  | x::xs ->
     if (p x)
     then (f x) :: xs
     else x :: (update_first p f xs)
