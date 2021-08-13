# This app presents code that breaks

app "BadAss"
    packages { base: "platform" }
    imports [base.Task, BadTest]
    provides [ main ] to base

main : Task.Task {} []
main =
    t1 = {name: "Addition", test: 1 + 1 == 2 }

    BadTest.run [t1] |>
       Task.putLine 

