type _ typ = Typ: 'a typ
let witness: type a. a -> a typ = (fun _ -> Typ)
let elem_typ: type a. a list typ -> a typ = (fun _ -> Typ)
