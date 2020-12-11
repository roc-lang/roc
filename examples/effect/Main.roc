app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [base.Task]
    provides [ main ] to base

main : Task.Task {} F64
main =
    Task.after (Task.getLine {}) \lineThisThing -> Task.putLine lineThisThing
