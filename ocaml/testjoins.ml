open Prelude
open Main

let testStreams = begin
  let e1 = Ev (0, (1,1)) in
  let e2 = Ev (2, (2,2)) in
  let e3 = Ev (4, (3,3)) in
  let e4 = Ev (6, (4,4)) in
  let s1 = toR([e1; e2; e3; e4]) in
  let e5 = Ev ("1", (5,5)) in
  let e6 = Ev ("3", (6,6)) in
  let e7 = Ev ("5", (7,7)) in
  let e8 = Ev ("7", (8,8)) in
  let s2 = toR([e5; e6; e7; e8]) in
  let e9 =  Ev (3.0, (9,9)) in
  let e10 = Ev (6.0, (10,10)) in
  let e11 = Ev (9.0, (11,11)) in
  let e12 = Ev (12.0, (12,12)) in
  let s3 = toR([e9; e10; e11; e12]) in
  let e13 = Ev (11, (8,9)) in
  let e14 = Ev (13, (9,10)) in
  let e15 = Ev (17, (10,11)) in
  let e16 = Ev (19, (11,12)) in
  let s4 = toR([e13; e14; e15; e16]) in
  (s1,s2,s3,s4)
end

let test1 correlation () =
  let (s1,_,_,_) = testStreams in
  let count = ref 0 in
  let show (Ev (a, (t1,t2))) =
    count := !count + 1;
    Printf.sprintf "%d. <%d@[%d,%d]>" !count a t1 t2
  in
  Async.run (fun () ->
      correlation show s1)

let test2 correlation () =
  let (s1,s2,_,_) = testStreams in
  let count = ref 0 in
  let show (Ev ((a,b), (t1,t2))) =
    count := !count + 1;
    Printf.sprintf "%d. <(%d,%s)@[%d,%d]>" !count a b t1 t2
  in
  Async.run (fun () ->
      correlation show s1 s2)

let test3 correlation () =
  let (s1,s2,s3,_) = testStreams in
  let count = ref 0 in
  let show (Ev ((a,b,c), (t1,t2))) =
    count := !count + 1;
    Printf.sprintf "%d. <(%d,%s,%.2f)@[%d,%d]>" !count a b c t1 t2
  in
  Async.run (fun () ->
      correlation show s1 s2 s3)

let test4 correlation () =
  let (s1,s2,s3,s4) = testStreams in
  let count = ref 0 in
  let show (Ev ((a,b,c,d), (t1,t2))) =
    count := !count + 1;
    Printf.sprintf "%d. <(%d,%s,%.2f,%d)@[%d,%d]>" !count a b c d t1 t2
  in
  Async.run (fun () ->
      correlation show s1 s2 s3 s4)

let cartesian1 (type a)
    (show: a  evt -> string)
    (s1: a evt r) () =
  let module T = struct type t0 = a
    type result = a
  end
  in
  let module J = Join1(T) in
  let module S = SingleWorld(struct type t = J.result end) in
  context
    show
    (fun () ->
       S.handler
         (J.correlate (fun () ->
              J.join s1 (function
                  | ev ->
                    S.yield ev))))

let testCartesian1 () = test1 cartesian1 ()

let cartesian2 (type a) (type b)
    (show: (a * b) evt -> string)
    (s1: a evt r)
    (s2: b evt r) () =
  let module T = struct type t0 = a
    type t1 = b
    type result = a * b
  end
  in
  let module J = Join2(T) in
  let module S = SingleWorld(struct type t = J.result end) in
  context
    show
    (fun () ->
       S.handler
         (J.correlate (fun () ->
              J.join (s1,s2) (function
                  | (Ev (x,i1),Ev (y,i2)) ->
                    S.yield (Ev ((x,y), i1 |@| i2))))))

let testCartesian2 () = test2 cartesian2 ()

let cartesian3 (type a) (type b) (type c)
    (show: (a * b * c) evt -> string)
    (s1: a evt r)
    (s2: b evt r)
    (s3: c evt r) () =
  let module T = struct type t0 = a
    type t1 = b
    type t2 = c
    type result = a * b * c
  end
  in
  let module J = Join3(T) in
  let module S = SingleWorld(struct type t = J.result end) in
  context
    show
    (fun () ->
       S.handler
         (J.correlate (fun () ->
              J.join (s1,s2,s3) (function
                  | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                    S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

let testCartesian3 () = test3 cartesian3 ()

let cartesian4 (type a) (type b) (type c) (type d)
    (show: (a * b * c * d) evt -> string)
    (s1: a evt r)
    (s2: b evt r)
    (s3: c evt r)
    (s4: d evt r)() =
  let module T = struct type t0 = a
    type t1 = b
    type t2 = c
    type t3 = d
    type result = a * b * c * d
  end
  in
  let module J = Join4(T) in
  let module S = SingleWorld(struct type t = J.result end) in
  context
    show
    (fun () ->
       S.handler
         (J.correlate (fun () ->
              J.join (s1,s2,s3,s4) (function
                  | (Ev (x,i1),Ev (y,i2),Ev (z,i3),Ev (w,i4)) ->
                    S.yield (Ev ((x,y,z,w), i1 |@| i2 |@| i3 |@| i4))))))

let testCartesian4 () = test4 cartesian4 ()

let affine3 (type a) (type b) (type c)
    (n: int)
    (i: int)
    (show: (a * b * c) evt -> string)
    (s1: a evt r)
    (s2: b evt r)
    (s3: c evt r) () =
  let module T = struct type t0 = a
    type t1 = b
    type t2 = c
    type result = a * b * c
  end
  in
  let module J = Join3(T) in
  let module S = SingleWorld(struct type t = J.result end) in
  context
    show
    (fun () ->
       S.handler
         (J.correlate
            ~restriction: (affine n (module J) i)
            (fun () ->
               J.join (s1,s2,s3) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                     S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))

let testAffine3_1_1 () = test3 (affine3 1 1) ()

let aligned3 (type a) (type b) (type c)
    (show: (a * b * c) evt -> string)
    (s1: a evt r)
    (s2: b evt r)
    (s3: c evt r) () =
  let module T = struct type t0 = a
    type t1 = b
    type t2 = c
    type result = a * b * c
  end
  in
  let module J = Join3(T) in
  let module S = SingleWorld(struct type t = J.result end) in
  context
    show
    (fun () ->
       S.handler
         (J.correlate
            ~restriction: (align3 (module J))
            (fun () ->
               J.join (s1,s2,s3) (function
                   | (Ev (x,i1),Ev (y,i2),Ev (z,i3)) ->
                     S.yield (Ev ((x,y,z), i1 |@| i2 |@| i3))))))


let testAlign3 () = test3 aligned3 ()
