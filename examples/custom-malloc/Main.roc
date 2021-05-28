app "custom-malloc-example"
    packages { base: "platform" }
    imports [ base.Task.{ Task } ]
    provides [ main ] to base

main : Task.Task {} []
main =
    _ <- Task.await (Task.putLine "About to allocate a list!")

    # This is the only allocation in this Roc code!
    # (The strings all get stored in the application
    # binary, and are never allocated on the heap.)
    list = [ 1, 2, 3, 4 ]

    if List.len list > 100 then
        Task.putLine "The list was big!"
    else
        Task.putLine "The list was small!"
