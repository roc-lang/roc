app "flappy-bird"
    packages { base: "platform" }
    imports [ base.SDL, base.Task.{ Task } ]
    provides [ gameContext ] to base

gameContext: SDL.Context
gameContext =
    { props: windowProperties, handler: handleEvent }

windowProperties : SDL.WindowConfig
windowProperties =
    { title: "Flappy Bird", width: 300, height: 73 }

handleEvent : {} -> Task {} []
handleEvent = \_ ->
    _ <- Task.await (Task.putLine "handling an event")
    Task.putLine "done"

