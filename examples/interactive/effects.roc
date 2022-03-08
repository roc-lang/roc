app "effects"
    packages { pf: "effects-platform" }
    imports [ pf.Effect ]
    provides [ main ] to pf

main : Effect.Effect {}
main =
    (Effect.putLine (Str.concat "It is known" "foo"))
