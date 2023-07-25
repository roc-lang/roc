platform "fuzz-glue-platform"
    requires {} {
        main : # {{ mainType }}
    }
    exposes []
    packages {}
    imports [
        # {{ imports }}
    ]
    provides [mainForHost]

mainForHost : # {{ mainForHostType }}
mainForHost =
    # {{ mainForHostBody }}
