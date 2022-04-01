hosted Effect
    exposes [ Effect, after, map, always, forever, loop, putLine, getLine ]
    imports []
    generates Effect with [ after, map, always, forever, loop ]

putLine : Str -> Effect {}

getLine : Effect Str
