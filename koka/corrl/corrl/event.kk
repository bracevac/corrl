public module corrl/event

import corrl/monoid

infixr 90 (|+|)

// --------------------------------------------------------------
// Time and events
// --------------------------------------------------------------
alias time = int //TODO can we have an unsigned type?
alias intrvl = (time, time)

type evt<a, t> {
  Ev(content: a, time: t)
}

alias ev<a> = evt<a, intrvl>

fun intrvlMonoid(action: () -> <monoid<intrvl>|e> a): <|e> a {
  handle (action) {
    return x -> x
    mempty() -> resume((1, 0))
    mappend(a, b) -> 
      match((a, b)) {
        ((a1, a2), (b1, b2)) -> 
          resume((a1.min(b1), a2.max(b2)))
      }
  }
}

//TODO for now we model the type class instance by an overloaded function
fun (|+|)(t1: intrvl, t2: intrvl): intrvl {
  val ((a1, a2), (b1, b2)) = (t1, t2)
  (a1.min(b1), a2.max(b2)) 
}


fun show(e: ev<int>): string {
  val Ev(x, (t1,t2)) = e
  "<" + x.show() + "@[" + t1.show() + "," + t2.show() + "]>"  
}

fun show(p: (int,int)): string {
  val (x,y) = p
  "(" + x.show() + "," + y.show() + ")"
}

fun show(e: ev<(int,int)>): string {
  val Ev(x, (t1,t2)) = e
  "<" + x.show() + "@[" + t1.show() + "," + t2.show() + "]>"  
}

fun show(e: ev<(int,string)>): string {
  val Ev((x,y), (t1,t2)) = e
  "<(" + x.show() + "," + y + ")@[" + t1.show() + "," + t2.show() + "]>"  
}

fun show(l: list<ev<int>>): string {
  show-list(l, show)
}
