effect Shift: (('a -> unit) -> unit) -> 'a

let shift f = perform (Shift f)

let reset thunk =
  try thunk () with
    effect (Shift f) k -> f (fun x -> continue k x)
