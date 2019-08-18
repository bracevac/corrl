include Handlers
include Data
include Evt
include Evt.Time
include Reactive

let println s = print_string s; print_string "\n"
let (+=) x n =
  x := !x + n
let (-=) x n =
  x := !x - n
let (+.=) x n =
  x := !x +. n
let (-.=) x n =
  x := !x -. n
