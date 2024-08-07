app [main] { pf: platform "effects-platform/main.roc" }

import pf.PlatformTask

main : Task {} []
main =
    line = PlatformTask.getLine!
    PlatformTask.putLine! "You entered: $(line)"
    PlatformTask.putLine! "It is known"

    Task.ok {}
