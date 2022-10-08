platform "cli"
    requires {} { main : InternalProgram }
    exposes []
    packages {}
    imports [Effect.{ Effect }, InternalProgram.{ InternalProgram }]
    provides [mainForHost]

mainForHost : Effect U8 as Fx
mainForHost = InternalProgram.toEffect main
