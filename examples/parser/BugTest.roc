# This app presents code that breaks

app "butTest"
    packages { base: "platform" }
    imports [base.Task, Bug, Test]
    provides [ main ] to base

main : Task.Task {} []
main =

    Test.run Bug.tests |>
       Task.putLine 

