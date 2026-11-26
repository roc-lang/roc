app [view] { pf: platform "./platform.roc" }

import pf.Scene

view : () -> [Free, Locked]
view = || Scene.empty
