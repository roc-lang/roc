app "fuzz-glue-app"
    packages { pf: "platform.roc" }
    imports [pf.Stdout]
    provides [main] to pf

main : # {{ mainType }}
main =
     # {{ mainBody }}
