app [main] { pf: platform "./platform/main.roc" }

import pf.Simple

# The platform's Model names this inferred nested tuple, not an app declaration.
main = {
    init: |{}| ({ value: 0.I64 }, ("store metadata", {})),
    update: |store, delta| ({ value: store.0.value + delta }, store.1),
    read: |store| store.0.value,
    render: |_store| Simple.leaf("hello"),
}
