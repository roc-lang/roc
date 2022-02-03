platform "folkertdev/foo"
    requires {} { main : Effect {} }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Unused {}

mainForHost : Task {} [] as Fx
mainForHost = main
