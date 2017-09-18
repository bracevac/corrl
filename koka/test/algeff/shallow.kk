effect nondet {
  fork(): bool
}

effect output<a> {
  out(x: a): ()
}

effect yield<a> {
  yield(x: a): ();
  cancel(): b
}

//Unsafe!!!
extern inline adjust : forall<a,b,e> (() -> <div,output<b>|e> a) -> (() -> e a) { inline "#1" }

fun bfs<a>(tasks: list<() -> <nondet,st<global>> maybe<a>>, 
           action: () -> <nondet,st<global>> maybe<a>): <div,nondet,output<a>,st<global>> () {
  handle shallow (action) {    
    return x -> 
      match(x) {
        Just(v) -> {
          out(v);          
          match(tasks) {
            Cons(thnk, tl) -> bfs(tl, thnk)            
            Nil -> ()            
          }
        }
        Nothing -> 
          match(tasks) {
            Cons(thnk, tl) -> bfs(tl, thnk)            
            Nil -> ()            
          }
      }

    fork() -> {      
      //TODO koka adds the effects of the current handler to resume's effects, which seems unnecessary.
      //We cheat by applying adjust to the alternatives
      val l = [adjust({resume(True)}), adjust({resume(False)})]
      match(tasks + l) {
        Cons(hd, tl) -> bfs(tl, hd)
        Nil -> () //this cannot happen, but we add it explicitly to prevent the exn effect from being added
      }
    }      
  }
}

val printer = handler<output<int>> {
  return x -> x
  out(i) -> {
    println(i)
    resume(())
  }
}

val asMaybe = handler<yield<int>> {
  return x -> Nothing
  yield(v) -> Just(v)
  cancel() -> Nothing
}

val unfork = handler {
  return x -> x
  fork() -> error("unhandled fork")
}

fun solutions(action: () -> <nondet,yield<int>,st<global>> ()) {
  unfork { //TODO why does the nondet effect escape?
    printer {
      bfs(Nil, {asMaybe(action)})
    }
  }
}

fun from<a,b>(xs: list<a>): <nondet,yield<int>> a {
  match (xs) {
    Cons(x,ys) -> if (fork()) then x else from(ys)
    Nil -> cancel()
  }
}

fun main() {  
  solutions {   
    val x = from([2,4,6,8,10])
    val y = from([1,3,5,7,9])  
    if (x > y) 
    then yield(x*y)     
    else cancel()
  }  
}