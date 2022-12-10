app "rust-glue"
    packages { pf: "platform/main.roc" }
    imports []
    provides [makeGlue] to pf

makeGlue = \_ ->
    Ok [
        {name: "amazing.rs", content: "This is the first file generated with roc glue!"}
    ]