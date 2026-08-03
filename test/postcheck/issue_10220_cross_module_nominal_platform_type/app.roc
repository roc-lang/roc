app [main] { pf: platform "platform/main.roc" }

import pf.Lib
import Wrapper

main = { things: [Box.box(handler!)] }

handler! = |_req| Wrapper.wrap("ok")
