module type SomeT = sig
  type t
end

let println s = print_string s; print_newline ()
