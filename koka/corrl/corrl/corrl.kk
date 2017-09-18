/*
 * The main module of the corrl language.
 * 
 * TODO: Generalize the definitions to support 
 * polymorphic streams.
 */
public module corrl

import std/async
import corrl/future
import corrl/event
import corrl/reactive
import corrl/equality
import corrl/pe
//import corrl/monoid

//What a correlation requires to run
alias toplevel = <exn,pure,console,st<global>,read<global>,alloc<global>,async,ndet,exn>

//Delimits the boundary between a single partial match and the surrounding manyWorld context
effect singleWorld<r> {
  yld(v: r): a;
  cncl(): a
}

//Default impl wraps successful ylds in maybe
fun singleWorld<r,a>(action: () -> <singleWorld<r>|e> a): <|e> maybe<r> {
  handle(action) {
    return x -> Nothing
    yld(v) -> Just(v)
    cncl() -> Nothing
  }
}

//A context that manages multiple match candidates
effect manyWorlds {
  fork(): bool
}

effect output<a> {
  out(x: a): b
}

fun printer(show: b -> string, action : () -> <output<b>,console|e> a): <console|e> a { handle(action) {
  return x -> x

  out(e) -> {
    println(show(e))
    resume(())
  }
}}

//Unsafe coercion trickery until we fix the type inference for shallow handlers
private extern inline adjustManyWorlds : forall<a,b,e> (() -> <div,output<b>|e> a) -> (() -> e a) { inline "#1" }

//Default impl schedules in round-robin fashion
fun manyWorlds<a>(worlds: list<() -> <manyWorlds,toplevel> maybe<a>>,
                  action: () -> <manyWorlds,toplevel> maybe<a>): <div,manyWorlds,output<a>,toplevel> () {
  handle shallow (action) {
    return x -> {
      interleaved({
      // wait(random()) // to witness the interleaving
      match(x) {
        Just(v) -> out(v)
        Nothing -> ()
      }
      },{
      match(worlds) {
        Cons(next, tl) -> manyWorlds(tl, next)
        Nil -> ()
      }
      })
      ()
    }

    fork() -> {
      val l = [adjustManyWorlds({resume(True)}), adjustManyWorlds({resume(False)})]
      match(worlds + l) { // swap for depth first search
        Cons(next, tl) -> manyWorlds(tl, next)
        Nil -> ()
      }
    }
  }
}

fun manyWorlds<a>(action: () -> <manyWorlds,toplevel> maybe<a>): <div,manyWorlds,output<a>,toplevel> () {
  manyWorlds(Nil, action)
}

val unfork = handler {
  return x -> x
  fork() -> error("Unhandled fork")
}

fun getref(r: ref<global,a>): <div,read<global>> a { !r }

fun getlast(r) {
  val (_, Just(v)) = getref(r)
  v
}

fun globalContext(show, action) {
      printer(show) {
        unfork {
          manyWorlds { action() }}}
}

fun correlate(pattern) {
  singleWorld {
    pattern() 
  }
}

fun testStreams() {
  val e1 = Ev(0, (1,1))
  val e2 = Ev(2, (2,2))
  val e3 = Ev(4, (3,3))
  val e4 = Ev(6, (4,4))
  val s1 = liftr([e1, e2, e3, e4])

  val e5 = Ev(1, (5,5))
  val e6 = Ev(3, (6,6))
  val e7 = Ev(5, (7,7))
  val e8 = Ev(7, (8,8))
  val s2 = liftr([e5, e6, e7, e8])

  (s1,s2)
}

fun testSpawn() {
  println("in the beginning ... ")
  val p = fr({wait(2.0); Ev(1, (0,0))})
  println("a promise was made ...")
  println(show(await(p)))
}

alias dir = bool

//The corrl effect reifies a (binary) join's state to be manipulated by handlers
effect corrl {
  corrl(st: (fr<react<int>>, fr<react<int>>), //current stream positions
        mt: (ref<global,list<(ev<int>,int)>>, ref<global,list<(ev<int>,int)>>), //mailboxes for left and right stream
        k: ((ev<int>,ev<int>)) -> <corrl,cps,manyWorlds,singleWorld<ev<int>>,toplevel> maybe<ev<int>>, //the body of the join binding
        x: ev<int>, //the latest event
        d: dir //we issue for left and right stream a separate join state
        ): maybe<ev<int>>
}

//Binary join binding
effect jn {
  jn(s1: fr<react<int>>, s2: fr<react<int>>): (ev<int>,ev<int>)
}

val left = True
val right = False

function sel(d: dir): (forall<a> ((a, a)) -> a)  {
  if (d) then fst else snd
}

function selother(d: dir): (forall<a> ((a, a)) -> a)  {
  if (!d) then fst else snd
}

fun make(d, x, y) {
  if (d) then (x,y) else (y,x)
}

function forkEach(l, f) {
  match(l) {
    Nil -> Nothing
    Cons(hd, tl) -> if (fork()) then f(hd) else forkEach(tl, f)
  }
}

fun eqEv(e1: ev<int>, e2: ev<int>) {
  val Ev(x1, (a1,b1)) = e1
  val Ev(x2, (a2,b2)) = e2
  x1==x2 && a1==a2 && b1==b2
}

val jn_assemble = handler { 
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val m2 = selother(d)(mt)
    m2 := map(!m2, (fun(yc) {
      val (y, c) = yc
      (y, c+1)
    }))
    val m1 = sel(d)(mt)
    m1 := map(!m1, (fun(yc) {
      val (y, c) = yc
      if (eqEv(y,x)) then (y, c + length(!m2)) else yc
    }))
    forkEach(!m2, (fun(yc) {
      val (y, _) = yc
      k(make(d, x, y))
    }))
    Nothing
  }
}

function forkEachStream(l: fr<react<int>>, f) {
  match(await(l)) {
    RNil -> Nothing
    RCons(hd, tl) -> if (fork()) then f(hd) else forkEachStream(tl, f)
  }
}

val jn_correlate = handler {
  return x -> x
  jn(s1, s2) -> {
    val m1 = ref(Nil)
    val m2 = ref(Nil)
    val k = resume
    if (fork()) then forkEachStream(s1, (fun(v) { corrl((s1,s2),(m1,m2),k,v,left) }))
    else forkEachStream(s2, (fun(v) { corrl((s1,s2),(m1,m2),k,v,right) }))
  }
}

val jn_forAll = handler(s: fr<react<int>>) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val sx = sel(d)(st)
    if (refEq(sx, s)) {
      val mx = sel(d)(mt)
      mx := Cons((x,0), !mx)
      corrl(st, mt, k, x, d)
    } else {
      resume(s, corrl(st, mt, k, x, d))
    }
  }
}

val jn_mostRecent = handler(s: fr<react<int>>) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val sx = sel(d)(st)
    if (refEq(sx, s)) {
      val mx = sel(d)(mt)
      mx := Cons((x,0), Nil)
      corrl(st, mt, k, x, d)
    } else {
      resume(s, corrl(st, mt, k, x, d))
    }
  }
}

val jn_affine = handler(s: fr<react<int>>) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val bx = refEq(sel(d)(st), s)
    val by = refEq(selother(d)(st), s)
    if (bx || by) {
      val (mx,my) = if (bx) 
      then (sel(d)(mt),selother(d)(mt)) 
      else (selother(d)(mt),sel(d)(mt))
      mx := filter(!mx, (fun(yc) {
        val (_, c) = yc
        c == 0
      }))
      my := (match(!my) {
        Nil -> Nil
        Cons(yc, _) -> Cons(yc, Nil)
      })
      corrl(st, mt, k, x, d)
    } else {
      resume(s, corrl(st, mt, k, x, d))
    }
  }
}

val jn_align = handler(ss) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val (s1,s2) = ss
    val (sx,sy) = st
    if ((refEq(sx, s1) && refEq(sy, s2)) ||
        (refEq(sx, s2) && refEq(sy, s1))) {
      val (m1, m2) = mt
      val a1 = !m1
      val a2 = !m2
      if (length(a1)>0 && length(a2)>0) {
        val Cons((e1,_),r1) = reverse(a1)
        val Cons((e2,_),r2) = reverse(a2)
        m1 := reverse(r1)
        m2 := reverse(r2)
        k((e1,e2))
      } else {
        Nothing
      }
    } else {
      resume(ss, corrl(st, mt, k, x, d))
    }
  }
}

val jn_pefilter = handler(s) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val (s1,s2,where) = s
    val (t1,t2) = st
    if (refEq(s1,t1) && refEq(s2,t2)) {
      on-cancel {
        println("cancelled "+show(x))
        val mx = sel(d)(mt)
        mx := filter(!mx, fun(yc) { val (y,_) = yc; !eqEv(x, y) })
        cncl()
      } {
      cancelable {
        val _ = firstof({val a = promise();
                        val b = promise();
                        val p = sel(d)((a,b));
                        resolve(p, x);
                        val guard:bool = where(a,b)
                        if (!guard) {cancel()};
                        ()},{wait(0.3);()});
        corrl(st, mt, fun(ab) {
          val a = promise()
          val b = promise()
          val (va,vb) = ab
          resolve(a, va)
          resolve(b, vb)
          val guard:bool = where(a,b)
          if (!guard) cncl() else k((va,vb))
        }, x, d)
      }}
    } else {
      resume(s, corrl(st, mt, k, x, d))
    }
  }
}

val jn_uncorrl = handler {
  return x -> x
  corrl(st, mt, k, x, d) -> error("Unhandled corrl")
}

fun testCartesianJoin() {
  val (s1,s2) = testStreams()
  globalContext(show) { correlate { jn_uncorrl {
    jn_assemble {
      jn_forAll(s1) {
        jn_forAll(s2) {
          jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            if (x < y) then
              yld(Ev((x+y), t1 |+| t2))
            else cncl()
  }}}}}}}
}

fun testPeJoin() {
  val (s1,s2) = testStreams()
  globalContext(show) { correlate { jn_uncorrl {
    jn_assemble {
      jn_pefilter((s1,s2,fun(x,y){iand({(?)(x) > 0},{(?)(y) > 1},{(?)(x) < (?)(y)})})) {
      jn_forAll(s1) {
        jn_forAll(s2) {
          jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            yld(Ev((x+y), t1 |+| t2))
  }}}}}}}}
}


fun testChallenge() {
  val (s1,s2) = testStreams()
  globalContext(show) { correlate { jn_uncorrl {
    jn_assemble {
      jn_affine(s2) {
      jn_affine(s1) {
      jn_mostRecent(s2) {
      jn_mostRecent(s1) {
      jn_forAll(s1) {
        jn_forAll(s2) {
          jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            yld(Ev((x+y), t1 |+| t2))
  }}}}}}}}}}}
}

fun testChallenge1() {
  val (s1,s2) = testStreams()
  globalContext(show) { correlate { jn_uncorrl {
    jn_assemble {
      jn_affine(s1) {
      jn_mostRecent(s2) {
      jn_mostRecent(s1) {
      jn_forAll(s1) {
        jn_forAll(s2) {
          jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            yld(Ev((x+y), t1 |+| t2))
  }}}}}}}}}}
}

fun testChallenge2() {
  val (s1,s2) = testStreams()
  globalContext(show) { correlate { jn_uncorrl {
    jn_assemble {
      jn_affine(s2) {
      jn_mostRecent(s2) {
      jn_mostRecent(s1) {
      jn_forAll(s1) {
        jn_forAll(s2) {
          jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            yld(Ev((x+y), t1 |+| t2))
  }}}}}}}}}}
}

fun testZipJoin() {
  val (s1,s2) = testStreams()
  globalContext(show) { correlate { jn_uncorrl {
    jn_assemble {
    jn_align((s1,s2)) {
      jn_forAll(s1) {
        jn_forAll(s2) {
          jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            yld(Ev((x+y), t1 |+| t2))
  }}}}}}}}
}

//TODO super evil, if only we had first class effects
extern inline (==): forall<a,b> (x: ref<global,a>, y: ref<global,b>) -> bool {
  js inline "#1 === #2"
}

fun refEq<a,b>(x: ref<global,a>, y: ref<global,b>): bool {
  x == y
}

fun window_indices(size, period, a) {
  // DUMB!
  var i := 0
  var c := 0
  var r := Nil
  while { c <= a } {
    if (a <= c+size) then { r := Cons(i, r) } else { }
    i := i+1
    c := c+period
  }
  reverse(r)
}

fun window_index_helper(ws, s, i, a) {
  match(s) {
   Nil -> {
     val r = (ref(Nil),ref(Nil))
     if (i==0) {
       ws := reverse(Cons(r,a))
       r
     } else {
       window_index_helper(ws, Nil, i - 1, Cons(r,a))
     }
   }
   Cons(hd, tl) -> if (i==0) then hd else window_index_helper(ws, tl, i - 1, Cons(hd,a))
 }
}

fun window_index(ws, i) {
  window_index_helper(ws, getref(ws), i, Nil)
}

fun window_find_helper(s, m1: ref<global,list<(ev<int>,int)>>, m2: ref<global,list<(ev<int>,int)>>, i) {
  match(s) {
  Nil -> error("window not found")
  Cons((hd1,hd2), tl) -> if ((hd1==m1) && (hd2==m2)) then i else window_find_helper(tl, m1, m2, i + 1)
  }
}

fun window_find(ws, mt) {
  val (m1,m2) = mt
  window_find_helper(getref(ws), m1, m2, 0)
}

val sliding_window = handler(c: (int,int,ref<global,list<(ref<global,list<(ev<int>,int)>>, ref<global,list<(ev<int>,int)>>)>>)) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val (size, period, ws) = c
    val Ev(_, (a,_)) = x
    val ns = window_indices(size, period, a)
    forkEach(ns, fun(i) {
      corrl(st, window_index(ws, i), k, x, d)
    })
  }
}

val add_window = handler(i: int) {
  return x -> x
  yld(v) -> { val Ev(_, t) = v; yld(Ev(i,t)) } // DUMB! this is just  a way to overwrite the result with the window, just to observe
  cncl() -> cncl()
}

// DUMB! except for the two lines with *window*, this is identical to jn_assemble
val assemble_windows = handler(ws: ref<global,list<(ref<global,list<(ev<int>,int)>>, ref<global,list<(ev<int>,int)>>)>>) {
  return x -> x
  corrl(st, mt, k, x, d) -> {
    val i = window_find(ws, mt)
    val m2 = selother(d)(mt)
    m2 := map(!m2, (fun(yc) {
      val (y, c) = yc
      (y, c+1)
    }))
    val m1 = sel(d)(mt)
    m1 := map(!m1, (fun(yc) {
      val (y, c) = yc
      if (eqEv(y,x)) then (y, c + length(!m2)) else yc
    }))
    forkEach(!m2, (fun(yc) {
      val (y, _) = yc
      add_window(i) { k(make(d, x, y)) }
    }))
    Nothing
  }
}

fun testWindow() {
  val (s1,s2) = testStreams()
  val ws = ref(Nil)
  globalContext(show) { correlate { jn_uncorrl {
    assemble_windows(ws) {
      jn_forAll(s1) {
        jn_forAll(s2) {
          sliding_window((5, 2, ws)) { jn_correlate {
            val (Ev(x, t1),Ev(y, t2)) = jn(s1,s2)
            yld(Ev((x+y), t1 |+| t2))
  }}}}}}}}
}
