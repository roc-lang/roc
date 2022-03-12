app "issue2279"
    packages { pf: "platform" }
    imports [ Issue2279Help, pf.Task ]
    provides [ main ] to pf

main =
    text =
        if True then
            Issue2279Help.text
        else
            Issue2279Help.asText 42

    Task.putLine text
