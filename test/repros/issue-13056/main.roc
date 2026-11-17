app [main] { pf: platform "https://github.com/lukewilliamboswell/roc-signals/releases/download/0.5.0/2SYUPo9uKFPJDvTyXiwsLEqHvmq7UMgaY4hdgMiSsSEF.tar.zst" }

import pf.Engine
import pf.Worker

main : Engine.Program
main = Engine.program(|| crash "declaration fixture unexpectedly initialized").with_workers([owner.registration([])])

owner : Worker.Preparation({}, {}, Worker.Never)
owner = Worker.preparation({ name: "empty-owner", prepare!: |_input| Ok({}) })
