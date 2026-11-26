platform ""
    requires { view : () -> [Free, Locked] }
    exposes [Scene]
    packages {}
    provides { "roc_view": view_for_host }
    targets: {
        x64musl: { inputs: [app], output: Archive },
    }

import Scene

# The app's `view` reads the polymorphic constant `Scene.empty` at an
# instance this call fixes; the call has no checked edge, so lowering derives
# the instance from its types and reads the root of the module holding it.
view_for_host : () -> [Free, Locked]
view_for_host = || view()
