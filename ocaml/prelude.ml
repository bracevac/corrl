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

let list_of_queue: 'a Queue.t -> 'a list = fun q ->
  let stack = Stack.create () in
  Queue.iter (fun x -> Stack.push x stack) q;
  Stack.fold (fun tl hd -> hd :: tl) [] stack

let rec take n xs = match n, xs with
  | 0, _ -> []
  | n, x :: xs when n > 0 -> x :: (take (n - 1) xs)
  | _, _ -> failwith "take: invalid arguments"
