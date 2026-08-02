platform ""
    requires {}
    exposes [Pages]
    packages {}
    provides {}
    hosted {
        "roc_list": Pages.list!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: [app] },
        arm64mac: { inputs: [app] },
        x64glibc: { inputs: [app] },
        arm64glibc: { inputs: [app] },
        x64musl: { inputs: [app] },
        arm64musl: { inputs: [app] },
        x64win: { inputs: [app] },
        arm64win: { inputs: [app] },
    }

import Pages
