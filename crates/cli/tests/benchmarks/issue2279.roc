app [main] { pf: platform "platform/main.roc" }

import Issue2279Help
import pf.PlatformTasks

main =
    text =
        if Bool.true then
            Issue2279Help.text
        else
            Issue2279Help.asText 42

    PlatformTasks.putLine text
        |> Task.mapErr! \_ -> crash "unreachable"
