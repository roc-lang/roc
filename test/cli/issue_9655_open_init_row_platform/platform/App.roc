App := [].{

    InitCallback(model) : {} => Try(model, [Exit(I64), ..])

    Init(model) : {
        config : {},
        run! : InitCallback(model),
    }

    init : InitCallback(model) -> Init(model)
    init = |callback!| { config: {}, run!: callback! }

}
