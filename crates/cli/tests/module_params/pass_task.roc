app [main] { pf: platform "../effects/platform/main.roc" }

import pf.PlatformTasks
import Menu { echo: PlatformTasks.putLine }

main =
    Menu.menu "Agus"
