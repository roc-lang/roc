app "issue2279"
    packages { pf: "platform" }
    imports [ Issue2279Help, pf.Task ]
    provides [ main ] to pf

main =
    t1 = Issue2279Help.asText 42
    t2 = Issue2279Help.text
    text = if True then t1 else t2

    Task.putLine text
