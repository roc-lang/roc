app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout

dothing = \fn ->
    Foo fn

main =
    with_extension "hi"

with_extension : Str -> Str
with_extension = |filename|
    answer = read_utf8! filename
    "$(filename).roc"

read_file! : Str => Str
read_file! = |path|
    read_utf8! (with_extension path) |> Result.with_default ""

read_utf8! : Str => Str
