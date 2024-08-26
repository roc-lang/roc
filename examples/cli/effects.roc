app [main] { pf: platform "effects-platform/main.roc" }

import pf.PlatformTasks

main : Task {} []
main =
    line =
        PlatformTasks.getLine
            |> Task.mapErr! \_ -> crash "unreachable"
    PlatformTasks.putLine "You entered: $(line)"
        |> Task.mapErr! \_ -> crash "unreachable"
    PlatformTasks.putLine "It is known"
        |> Task.mapErr! \_ -> crash "unreachable"

    Task.ok {}
