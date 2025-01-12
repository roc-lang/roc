app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import Alias { passed: \_ -> Ok() }

main =
    Task.loop!((), loop)

loop = \() ->
    Task.map(Alias.exposed, \x -> Done(x))
