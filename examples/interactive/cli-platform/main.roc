platform "cli"
    requires {} { main : InternalProgram }
    exposes []
    packages {}
    imports [Effect.{ Effect }, InternalProgram.{ InternalProgram }]
    provides [mainForHost]

mainForHost : List Str -> Effect U8 as Fx
mainForHost = \args -> InternalProgram.toEffect main args
