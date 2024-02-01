interface Program
    exposes [Program]

Program model : {
    init : {} -> model,
    update : model, Str -> model,
    view : model -> Str,
}
