platform "tui"
    requires { Model } { main : { init : () -> Model, update : Model, Str -> Model, view : Model -> Str } }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : { init : (() -> Model) as Init, update : (Model, Str -> Model) as Update, view : (Model -> Str) as View }
main_for_host = main
