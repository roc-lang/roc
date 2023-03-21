app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3/5CcipdhTTAtISf4FwlBNHmyu1unYAV8b0MKRwYiEHys.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    Stdout.line "Hello, World!"
