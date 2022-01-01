app "issue2279"
    packages { pf: "platform" }
    imports [ Issue2279Help, pf.Task ]
    provides [ main ] to pf

main =
    text = Issue2279Help.text

    Task.putLine text
