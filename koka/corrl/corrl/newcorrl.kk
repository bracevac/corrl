public module newcorrl

import std/async
import corrl/future
import corrl/event
import corrl/reactive
import corrl/equality
import corrl/pe

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

effect push3<a,b,c> {
  push1(v: a): ();
  push2(v: b): ();
  push3(v: c): ()
}

type count {
  Fin(n: int)
  Inf
}

alias mailbox<a> = list<(a,ref<global,count>)>

effect mailboxes3<a,b,c> {
  getmail1(): mailbox<a>;
  getmail2(): mailbox<b>;
  getmail3(): mailbox<c>;
  setmail1(v: mailbox<a>): ();
  setmail2(v: mailbox<b>): ();
  setmail3(v: mailbox<c>): ();
}

val forAll = handler {
  return x -> x

  push1(x) -> {
    setmail1(Cons((x,ref(Inf)), getmail1()))
    resume(push1(x))
  }
  push2(x) -> {
    setmail2(Cons((x,ref(Inf)), getmail2()))
    resume(push2(x))
  }
  push3(x) -> {
    setmail3(Cons((x,ref(Inf)), getmail3()))
    resume(push3(x))
  }
}

val mostRecent1 = handler {
  return x -> x

  push1(x) -> {
    setmail1([(x,ref(Inf))])
    resume(push1(x))
  }
  push2(x) -> {
    resume(push2(x))
  }
  push3(x) -> {
    resume(push3(x))
  }
}

val mostRecent2 = handler {
  return x -> x

  push1(x) -> {
    resume(push1(x))
  }
  push2(x) -> {
    setmail2([(x,ref(Inf))])
    resume(push2(x))
  }
  push3(x) -> {
    resume(push3(x))
  }
}

val mostRecent3 = handler {
  return x -> x

  push1(x) -> {
    resume(push1(x))
  }
  push2(x) -> {
    resume(push2(x))
  }
  push3(x) -> {
    setmail3([(x,ref(Inf))])
    resume(push3(x))
  }
}
