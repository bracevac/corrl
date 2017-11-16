public module corrl/future

import std/async
import corrl/equality

alias fr<a> = promise<a>

fun fr<a>(thunk: () -> <async,exn,ndet|e> a): <async,exn,ndet|e> fr<a> {
  val p = promise()
  fork({resolve(p, thunk())})
  p
}

//We require equality on futures in order to bind a given handler to a specific stream
extern inline (===): forall<a,b> (x: a, y: b) -> bool {
  js inline "#1 === #2"
}

fun refEq<a,b>(x: a, y: b): bool {
  x === y
}

fun futureEq<a>(action: () -> <equality<fr<a>>|e> a): <|e> a {
  handle(action) {
    return x -> x
    equal(a, b) -> resume(refEq(a,b))
  }
}