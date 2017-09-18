public module corrl/equality

//TODO adapt this to type class encoding akin to corrl/typeclassdemo
effect equality<t> {
  equal(a: t, b: t): bool
}