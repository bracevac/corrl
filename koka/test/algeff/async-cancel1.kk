// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val name = timeout(2.seconds) {
    println("what is your name?")
    readline()
  }
  println("hi '" + name.default("<timed out>") + "'")
}