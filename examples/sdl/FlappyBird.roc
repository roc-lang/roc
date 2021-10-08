app "flappy-bird"
    packages { base: "platform" }
    imports [ base.SDL, base.Task.{ Task } ]
    provides [ main ] to base


main : Task {} []
main =
    _ <- Task.await SDL.init
    window <- Task.await SDL.createWindow
    renderer <- Task.await (SDL.createRenderer window)
    _ <- Task.await (SDL.eventLoop render)

    Task.putLine "window opened"

