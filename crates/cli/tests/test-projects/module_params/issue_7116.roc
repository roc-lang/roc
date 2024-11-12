app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
}

import Alias { passed: Task.ok {} }

main =
    Task.loop! {} loop

loop = \{} ->
    Task.map Alias.exposed \x -> Done x
