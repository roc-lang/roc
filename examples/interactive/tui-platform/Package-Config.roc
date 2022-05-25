platform "tui"
    requires { Model } { main : { init : {} -> Model, update : Model, Str -> Model, view : Model -> Str } }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : { init : ({} -> Model) as Init, update : (Model, Str -> Model) as Update, view : (Model -> Str) as View }
mainForHost = main
