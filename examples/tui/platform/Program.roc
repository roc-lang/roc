interface Program
    exposes [ Program ]
    imports []

Program model :
    {
        init : {} -> model,
        update : model, Str -> model,
        view : model -> Str,
    }
