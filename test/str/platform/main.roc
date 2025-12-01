platform ""
    requires {} { process_string : Str -> Str }
    exposes []
    packages {}
    provides { process_string_for_host: "process_string" }

process_string_for_host : Str -> Str
process_string_for_host = process_string
