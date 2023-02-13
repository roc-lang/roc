platform "arena-alloc"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [Task.{ Task }]
    provides [mainForHost]

mainForHost : Task {} [] as Fx
mainForHost = main
