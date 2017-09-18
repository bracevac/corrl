public module corrl/reactive

import std/async
import corrl/event
import corrl/future

// --------------------------------------------------------------
// Reactives
// --------------------------------------------------------------


rectype reactive<a,t> {
  RCons(rhead: evt<a,t>, rtail: fr<reactive<a,t>>)
  RNil
}

alias react<a> = reactive<a,intrvl>

fun take<a,t>(r: reactive<a,t>, n: int): <div,read<global>,async,exn,ndet> list<evt<a,t>> {
  if (n <= 0) 
  then Nil
  else match (r) {
    RNil -> Nil
    RCons(hd, tlp) -> Cons(hd, take(await(tlp), n - 1))
  }
}

fun show(r: react<int>): <div,read<global>,async,exn,ndet> string {
  show-list(r.take(100), show)  
}

fun show(r: react<(int,int)>): <div,read<global>,async,exn,ndet> string {
  show-list(r.take(100), show)  
}

fun show(r: ref<global, react<int>>): <read<global>,div,async,exn,ndet> string {
  show-list((!r).take(100), show)
}

fun show(r: ref<global, react<(int,int)>>): <read<global>,div,async,exn,ndet> string {
  show-list((!r).take(100), show)
}

fun liftr<a>(l: list<ev<a>>): <async,exn,ndet> fr<react<a>> {
  match (l) {
    Nil -> fr({RNil})
    Cons(x, xs) -> fr({RCons(x, liftr(xs))})
  }
}

