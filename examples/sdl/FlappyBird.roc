app "flappy-bird"
    packages { base: "platform" }
    imports [ base.SDL, base.Task.{ Task } ]
    provides [ handleEvent, windowProperties ] to base

windowProperties : SDL.WindowConfig
windowProperties =
    { title: "Flappy Bird", width: 300, height: 73 }

handleEvent : Task {} []
handleEvent =
    _ <- Task.await (Task.putLine "handling an event")
    Task.putLine "done"

