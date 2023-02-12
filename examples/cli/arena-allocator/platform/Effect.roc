hosted Effect
    exposes [Effect, arenaStart, arenaEnd, stdinRead, stdoutWrite, always, after]
    imports []
    generates Effect with [always, after]

arenaStart : Effect U8
arenaEnd : Effect {}
stdinRead : Effect Str
stdoutWrite : Str -> Effect {}
