platform "effects"
    requires {} { main : Effect.Effect {} }
    exposes []
    packages {}
    provides [mainForHost]

import Effect

mainForHost : Effect.Effect {}
mainForHost = main
