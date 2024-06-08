app "type-error"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }
    imports [pf.Task.{ Task }]
    provides [main] to pf

main = \_ ->
    "this is a string, not a Task {} [] function like the platform expects."