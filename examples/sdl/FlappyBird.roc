app "flappy-bird"
    packages { base: "platform" }
    imports [ base.Window.{ Window }, base.Task.{ Task } ]
    provides [ main ] to base


main : Task {} []
main =
    _ <- Task.attempt Window.createWindow

    Task.putLine "window opened"

