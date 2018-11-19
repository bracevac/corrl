module type monad = sig
  type 'a m
  val return: 'a -> 'a m
  val (>>=): 'a m -> ('a -> 'b m) -> 'b m
end

module Identity = struct
  type 'a m = 'a
  let return x = x
  let (>>=) m f = f m
end

module ListMonad = struct
  type 'a m = 'a list
  let return x = [x]
  let (>>=) m f = List.concat @@ List.map f m
end
