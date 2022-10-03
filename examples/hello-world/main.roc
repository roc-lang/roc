app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

crash : Str -> a
crash = \msg ->
  expect msg != msg

  diverge : {} -> a
  diverge = \{} -> diverge {}

  diverge {}

main = crash "Hello, World!\n"
