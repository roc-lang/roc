app "effect-example"
    packages { pf: "thing/platform-dir" }
    imports [ pf.Effect ]
    provides [ main ] to pf

main : Effect.Effect {}
main =
    (Effect.putLine (Str.concat "It is known" "foo"))
