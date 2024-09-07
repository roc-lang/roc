app [main] { pf: platform "platform/main.roc" }

import pf.PlatformTasks

main : Task {} []
main =
    line = PlatformTasks.getLine!
    PlatformTasks.putLine! "You entered: $(line)"
    PlatformTasks.putLine! "It is known"

    Task.ok {}
