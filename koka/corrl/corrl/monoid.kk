public module corrl/monoid

infixr 90 (|+|)

//TODO adapt this to type class encoding akin to corrl/typeclassdemo
effect monoid<m> {
  mappend(t1: m, t2: m): m;
  mempty(): m
}

fun (|+|)<m>(x: m, y: m): <monoid<m>> m {
  mappend(x,y)
}