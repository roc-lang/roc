platform "folkertdev/foo"
    requires {} { main : Effect {} }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]

mainForHost : Task {} [] as Fx
mainForHost = main
