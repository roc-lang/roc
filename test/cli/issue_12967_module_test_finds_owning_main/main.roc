app [main] { pf: platform "platform/main.roc" }

import Counter
import Unrelated

main = Counter.label(Unrelated.value)
