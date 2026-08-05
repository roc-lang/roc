app [main!] { pf: platform "../../fx/platform/main.roc" }

import Alias

LocalResult : Alias.Result

main! = || {
    _ = Alias.Result.wrap("John")
    _ = LocalResult.wrap("Jane")

    {}
}
