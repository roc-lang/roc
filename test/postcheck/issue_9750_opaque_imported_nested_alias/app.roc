app [Model, program] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/9750
#
# `Player` is an app-defined opaque nominal whose implements block declares a
# method returning `Player`, and whose backing record references `Types.Animation`
# — a nested alias imported from the exposed platform module `Types`. `roc check`
# accepted this, but `roc build` panicked in postcheck monotype lowering with
# "imported nominal declaration formal was not projected into the current checked
# type store" because the opaque's box-payload-capability backing (and its nested
# imported alias) was not projected into the platform's checked type store.
#
# `Player.new` is never called; merely declaring it in the implements block is
# enough to trigger the lowering path. The build must succeed.

import pf.Types

Player := {
    animation : Types.Animation,
}.{
    new : {} -> Player
    new = |_| {
        animation: { frame_count: 4, fps: 10 },
    }
}

Model : Player

program = { init: init }

init : Model
init = { animation: { frame_count: 4, fps: 10 } }
