import Host exposing [Host]

App := [].{
    Config : {}

    default : Config
    default = {}

    Init(state) : {
        config : Config,
        run! : Host => Try(state, [Exit(I64)]),
    }

    init : Config, (Host => Try(state, [Exit(I64)])) -> Init(state)
    init = |config, run!| { config, run! }
}
