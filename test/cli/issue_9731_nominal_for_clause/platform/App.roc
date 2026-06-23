App := [].{
    Init(model) : { config : {}, run! : {} => model }
    init : ({} => model) -> Init(model)
    init = |callback!| { config: {}, run!: callback! }
}
