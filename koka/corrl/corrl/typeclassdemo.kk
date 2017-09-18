public module corrl/typeclassdemo

import corrl/equality
import corrl/future

//Emulation of type classes/type-specific handling with open types and handlers

//Example: awaiting futures

//Extend this type for all the type cases that you want to handle
type open awaitImpl;

//The effect for awaiting a future, to avoid name clashes, the operation has Op as suffix
effect await {
  awaitOp(f: awaitImpl): a
}

//Awaiting int values
type extend awaitImpl {
  AwaitInt(futint: ref<global,int>)
}

//Client code should call the overloaded function await, which hides the underlying open type
fun await(f: ref<global,int>): <await|e> int {
  awaitOp(AwaitInt(f))
}

//Another extension for strings
type extend awaitImpl {
  AwaitString(futstring: ref<global,string>)
}

fun await(f: ref<global,string>): <await|e> string {
  awaitOp(AwaitString(f))
}

//Consequence: need a top level catch-all handler
val unawait = handler<await> {
  return x -> x
  awaitOp(f) -> error("no instance for await")
}

//A specific implementation of await for int values
val awaitInt = handler<await> {
  return x -> x
  awaitOp(f) -> match(f) {
    AwaitInt(_) -> resume(1337)    
    _ -> resume(awaitOp(f)) //delegate other cases to outer context
  }
}

//Dito for string
val awaitString = handler<await> {
  return x -> x
  awaitOp(f) -> match(f) {
    AwaitString(fr) -> resume(!fr)    
    _ -> resume(awaitOp(f))
  }
}

fun main() {  //TODO can we have an automatic resolution mechanism?
  unawait {
    awaitString {
      awaitInt {
        val f1: ref<global,int> = ref(0)
        val f2: ref<global,string> = ref("foo")
        
        await(f2) + " is " + show(await(f1))
      }
    }
  }
}

