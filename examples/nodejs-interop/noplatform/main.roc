app [main] { pf: platform "platform/main.roc" }


add = \a, b -> a + b

main : Str
main =
  count = add 1 2
  "Hello from Roc! $(Inspect.toStr count)"
