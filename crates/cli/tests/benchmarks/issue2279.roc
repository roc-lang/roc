app "issue2279"
    packages { pf: "platform/main.roc" }
    imports [Issue2279Help, pf.Task]
    provides [main] to pf

main =
    text =
        if Bool.true then
            Issue2279Help.text
        else
            Issue2279Help.asText 42

    Task.putLine text
