app [main!] {
    pf: platform "./platform/main.roc",
    wrapper: "./platform_wrapper/main.roc",
}

import wrapper.FxWrapper

main! = || {
    host : FxWrapper.WrappedHost
    host = FxWrapper.new("Package")
    FxWrapper.print_greeting!(host)
}
