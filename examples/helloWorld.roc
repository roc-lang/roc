app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3/OBKa1ehApuKND8nt6-1aAgFEJNDPCskCQ1oANjNH7Cg.tar.gz" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    Stdout.line "Hello, World!"
