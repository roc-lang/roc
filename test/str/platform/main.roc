platform ""
    requires {} { process_string : Str -> Str }
    exposes []
    packages {}
    provides { process_string_for_host: "process_string" }
    targets: {
        exe: {
            x64mac: [app],
            arm64mac: [app],
            x64musl: [app],
            arm64musl: [app],
            x64glibc: [app],
            arm64glibc: [app],
        }
    }

process_string_for_host : Str -> Str
process_string_for_host = process_string
