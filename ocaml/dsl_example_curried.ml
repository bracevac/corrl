open Symanticscurried


(* TODO: make variation using Oleg's dptr example *)
module InjectionExample = struct
  open Hlists

  (* meta data*)
  type  meta = int
  let merge: meta -> meta -> meta = fun x y -> x*y
  module MCtx = HList(struct type 'a t = meta end)
  module MFold = HFOLD(MCtx)
  let merge_hlist metas =
    MFold.(fold { zero = 1; succ = fun m next -> merge m (next ()) } metas)

  type 'a repr = 'a
  type 'a shape = ('a * meta) list
  type 'a elem = 'a * int
  type 'a el_pat = 'a repr * meta repr

  let elem x = x
  let el_pat x = x

  (* TODO: can we eliminate this boilerplate somehow *)
  include StdContextRepr(struct type 'a repr = 'a type meta = int type 'a shape = ('a * meta) list
                                type 'a elem = 'a * int type 'a el_pat = 'a repr * meta repr end)

  (* expressions *)
  let pair: 'a repr -> 'b repr -> ('a * 'b) repr  = fun x y -> (x,y)
  let bool: bool -> bool repr = fun b -> b

  (* patterns *)
  type ('ctx, 'a) pat = 'ctx MCtx.hlist -> 'a shape
  let zero: ('ctx, 'a) pat = fun _ -> []
  let where: bool repr -> ('m, 'a) pat -> ('m, 'a) pat = fun b p -> if b then p else zero
  let yield: 'a repr -> ('m, 'a) pat = fun v ctx -> [(v, merge_hlist ctx)]

  (* In this example, extensions depend on the extracted tuples.
     Alternatives: depend on the whole shape context*)
  module ECtx = HList(struct type 'a t = 'a el_pat end)
  type 'ctx ext = 'ctx ECtx.hlist -> unit
  let empty_ext: 'a ext = fun _ -> ()
  let (|++|) f g ctx = (f ctx); (g ctx)

  let rec cart : type s r ps w. (s, (ps * r)) ctx -> ps -> (s ECtx.hlist -> s MCtx.hlist -> r -> w list) -> w list = fun hs g f ->
    match hs with
    | (Z, Ctx.Z) -> f ECtx.nil MCtx.nil g
    | (S n, Ctx.S (Bind ls, hs)) ->
       let bind f = List.(concat @@ map f ls) in
       bind (fun el_rep ->
           cart (n,hs) (g el_rep) (fun ectx_tail mctx_tail ->
               f (ECtx.cons el_rep ectx_tail) (MCtx.cons (snd el_rep) mctx_tail)))

  let join: type s ps b. (s, (ps * (s,b) pat)) ctx -> s ext -> ps -> b shape repr =
    (fun ctx ext curried_body ->
      cart ctx curried_body (fun ectx mctx bound_body ->
          ext ectx; bound_body mctx))

  (* Example dptr-based extensions: *)
  module ECtxP = HListP(ECtx)
  let show: type i ctx. (i el_pat -> string) -> (i,ctx) ptr -> ctx ext =
    fun format ptr ctx ->
    let v_i = ECtxP.proj ptr ctx in
    print_string (format v_i)
  let msg: string -> 'ctx ext = fun msg _ -> print_string msg

  let show_string () = show (fun (v,t) -> Printf.sprintf "(\"%s\",%d)" v t)
  let show_int () = show (fun (v,t) -> Printf.sprintf "(%d,%d)" v t)
  let show_float () = show (fun (v,t) -> Printf.sprintf "(%f,%d)" v t)
end

module TestInject(S: JoinExtSym) = struct
  open S
  let binders a b c =
    ((from a) @. (from b) @. (from c) @. cnil)
  let exp a b i =
    join ((from a) @. (from b) @. cnil)
      i
      (fun (x,_) (y,_) -> (* TODO try to get curried version *)
        (where (bool true)
           (yield (pair x y))))
end

module TestInst = struct
  include TestInject(InjectionExample)
  open InjectionExample
  open Hlists
  let test () =
    let ext = (msg "extension:\n")
              |++| (show_int () (Ptrs.n0 ()))
              |++| (msg " ")
              |++| (show_string () (Ptrs.n1 ()))
              |++| (msg "\n") in
    let s = exp [(1,1);(2,3);(3,5)] [("a",2);("b",4);("c",6);("d",8)] ext in
    let s2 =
      join ((from s) @. (from [(3.1,10)]) @. cnil)
        empty_ext
        (fun (x,_) (y,_) ->
          yield (pair x y))
    in s2
end
