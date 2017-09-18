import std/async
import corrl/event

fun (?)(p: promise<ev<a>>): <asyncx|e> a  {
  await(p).content
}

//TODO make this a nestable infix binary op
fun iand(a: () -> <asyncx|e> bool, b: () -> <asyncx|e> bool): <asyncx|e> bool {
  conjunction-interleaved(vector([a,b]))
}

fun iand(a: () -> <asyncx|e> bool, b: () -> <asyncx|e> bool, c: () -> <asyncx|e> bool): <asyncx|e> bool {
  conjunction-interleaved(vector([a,b,c]))
}

fun iand(a: () -> <asyncx|e> bool, b: () -> <asyncx|e> bool, c: () -> <asyncx|e> bool, d: () -> <asyncx|e> bool): <asyncx|e> bool {
  conjunction-interleaved(vector([a,b,c,d]))
}

//adapted from koka's std/async module
//TODO would be nice to generalize from bool to an arbitrary group type
fun conjunction-interleaved( xs : vector<() -> <asyncx|e> bool> ) : <asyncx|e> bool {
  async-handle-interleaved {
    val n = xs.length
    if (n==0) return True
    var count := n;
    val i = primforkn(n)
    val x: bool = inject-st(xs[i])()
    if (!x) then {
      cancel();
      False
    } else {
      count := count - 1
      if (count>0) { exit() }
      True
    }
  }
}