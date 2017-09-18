public module corrl2

import std/async
import corrl/future
import corrl/event
import corrl/reactive
import corrl/equality
import corrl/pe
//import corrl/monoid

infixr 89 (|&&|)

type cmdt<a,t> {
  Bind(event: evt<a,t>)
  Skip(next: fr<reactive<a,t>>)
}

alias cmd<a> = cmdt<a,intrvl>

effect iteration { 
  snext(r: fr<react<a>>): cmd<a>
}

private extern inline unsafeCast: forall<a,b> ((a, b) -> b) { inline "#1" }

fun from(stream: fr<react<a>>): <iteration|e> ev<a> {
  match (snext(stream)) {
    Bind(evt) -> evt
    Skip(next) -> from(next) 
  }
}

val uniteration = handler {
  return x -> x
  snext(f) -> error("Unhandled snext")
}

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

fun printer(shw: b -> string, action : () -> <output<b>,console|e> a): <console|e> a { 
  var i := 0
  handle(inject-st(action)) {
    return x -> x

    out(e) -> {
      println(i.show + ": " + shw(e))
      i := i + 1
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
      match(worlds + l) { // swap between depth first and breath first
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

fun forAll(currentStream: fr<react<a>>, 
           action: () -> <iteration,manyWorlds,singleWorld<b>,asyncx|e> c): <iteration,manyWorlds,singleWorld<b>,asyncx|e> c {
  handle (action) {
    return x -> x

    snext(f) ->
      if (refEq(f, currentStream))
      then {
        val futHead = unsafeCast(currentStream, f)
        match(await(futHead)) {
          RCons(v, tl) -> {
            val fwd = unsafeCast(tl, currentStream)
            forAll(fwd) {
              resume(if (fork()) then Bind(v) else Skip(tl))
            }            
          }

          RNil -> cncl()
        }
      }
      else {
        val outer = snext(f)
        forAll(currentStream) {
          resume(outer)
        } 
      }
  }
}

fun globalContext(show, action) {
      printer(show) {
        unfork {
          manyWorlds { action() }}}
}

fun correlate(pattern) {
  singleWorld {
    uniteration {
      pattern()
    }
  }
}

// fun copies(n: int, 
//            current: fr<react<a>>, 
//            action: () -> <iteration,manyWorlds,singleWorld<b>,asyncx|e> c) {
//   var count := n
//   val guardYld = handler {
//     return x -> x 

//     yld(v) -> 
//       if (count > 0) 
//       then {
//         count := count - 1
//         resume(yld(v)) //Beware: we don't know what the outer handlers do
//       }
//       else resume(cncl()) //TODO: how to cancel all pending duplicates at once?

//     cncl() ->     
//       resume(cncl())    
//   }

//   guardYld {  
//     handle shallow (action) {
//       return x -> x

//       snext(f) ->
//         if (refEq(f, current))
//         then {
//           if (count > 0) 
//           then {
//            val outer = snext(f)
//            copies(n, current) {
//              resume(outer)
//            }
//           }
//           else cncl()        
//         }
//         else {
//           val outer = snext(f)
//           copies(n, current) {
//             resume(outer)
//         }
//       }
//     }
//   }
// } 

fun testStreams() {
  val e1 = Ev(0, (1,1))
  val e2 = Ev(2, (2,2))
  val e3 = Ev(4, (3,3))
  val e4 = Ev(6, (4,4))
  val s1 = liftr([e1, e2, e3, e4])

  val e5 = Ev("one", (5,5))
  val e6 = Ev("three", (6,6))
  val e7 = Ev("five", (7,7))
  val e8 = Ev("seven", (8,8))
  val s2 = liftr([e5, e6, e7, e8])

  (s1,s2)
}

fun testCartesian() {
  val (s1,s2) = testStreams()
  globalContext(show) {
    correlate {
      forAll(s1) {
        forAll(s2) {
          val Ev(x, t1) = from(s1)
          val Ev(y, t2) = from(s2)         
          yld(Ev((x,y), t1 |+| t2))          
  }}}}
}

fun testCartesianAlias() {
  val (s1,_) = testStreams()
  globalContext(show) {
    correlate {
      forAll(s1) {
        forAll(s1) {
          val Ev(x, t1) = from(s1)
          val Ev(y, t2) = from(s1)
          yld(Ev((x,y), t1 |+| t2))
  }}}}
}

fun testFirstOf() {
  val x = promise()
  var res := Nothing

  interleaved {firstof { wait(random()); println("left"); await(x); res := Just("left") }
                       { wait(random()); println("right"); await(x); res := Just("right")}}
              { wait(random()); println("resolve"); resolve(x, True) }
  res            
}

fun show(x: maybe<string>): string {
  match(x) {
    Nothing -> "Nothing"
    Just(s) -> "Just(" + s + ")"
  }
}

//TODO support more arities
effect join2<a,b> { //TODO cancellation and copying as part of this interface?
  getJoinState(): (promise<a>, promise<b>);
  completeLeft(value: a): ();
  completeRight(value: b): ()
}

val hJoin2 = handler (st = (promise(), promise())) { //problem: this should be per strand?
  return x -> x
  getJoinState() -> resume(st, st)
  completeLeft(v) -> {
    val (left, _) = st
    //TODO should this be interleaved with resumption?  
    catch {
      resolve(left, v)
    }
    fun(exn) {
      match(exn.info) {
        Error -> () //ignore double resumption
        _      -> throw(exn)
      }
    }
    resume(st, ())
  }
  completeRight(v) -> {
    val (_, right) = st
    //TODO should this be interleaved with resumption?  
    catch {
      resolve(right, v)
    }
    fun(exn) {
      match(exn.info) {
        Error -> () //ignore double resumption
        _      -> throw(exn)
      }
    }
    resume(st, ())
  }
}

fun join2(left, 
          right,
          where = fun(a,b){ True }) {
  on-cancel {
    cncl()
    } {
    cancelable {
      val (a,b) = getJoinState()
      val _ = interleaved([{val x = left();  completeLeft(x) }, 
                           {val y = right(); completeRight(y)}, 
                           {val guard:bool = where(a, b)
                            if(!guard) {cancel()} 
                            ()}])    
      (await(a), await(b))
    }
  }
}

fun testCartesianNewJoins() {
  val (s1,s2) = testStreams()
  globalContext(show) {
    correlate {
      forAll(s1) {
        forAll(s2) {
          hJoin2 {
            val (Ev(x, t1), Ev(y, t2)) = join2({from(s1)},
                                               {from(s2)}, 
                                               fun(x,y){
                                                 iand({(?)(x) > 0}, {(?)(x) != count((?)(y):string)})})
            yld(Ev((x,y), t1 |+| t2))          
          }
  }}}}
}

//TODO implement zip_recent
