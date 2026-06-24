platform ""
    requires {
        main! : () => {}
    }
    exposes [Stdout, Stderr, Stdin, Builder, Host, NodeA, NodeB, Element, Padded]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_builder_print_value": Builder.print_value!,
        "roc_host_boxed_add": Host.boxed_add!,
        "roc_host_boxed_drop_report": Host.boxed_drop_report!,
        "roc_host_boxed_nested_record": Host.boxed_nested_record!,
        "roc_host_boxed_recursive_tree": Host.boxed_recursive_tree!,
        "roc_host_boxed_with_boxed_capture": Host.boxed_with_boxed_capture!,
        "roc_host_call_boxed": Host.call_boxed!,
        "roc_host_get_greeting": Host.get_greeting!,
        "roc_host_release_stored_boxed": Host.release_stored_boxed!,
        "roc_host_reset_boxed_drop_report": Host.reset_boxed_drop_report!,
        "roc_host_roundtrip_boxed": Host.roundtrip_boxed!,
        "roc_host_store_boxed": Host.store_boxed!,
        "roc_host_stored_boxed_call": Host.stored_boxed_call!,
        "roc_padded_check": Padded.check!,
        "roc_stderr_line": Stderr.line!,
        "roc_stdin_line": Stdin.line!,
        "roc_stdout_line": Stdout.line!,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
    }

import Stdout
import Stderr
import Stdin
import Builder
import Host
import NodeA
import NodeB
import Element
import Padded

main_for_host! : () => {}
main_for_host! = main!
