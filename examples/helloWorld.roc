app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    { foobar: k2 } = { foobar: 1234 }

    Stdout.line "Hello, World!"
