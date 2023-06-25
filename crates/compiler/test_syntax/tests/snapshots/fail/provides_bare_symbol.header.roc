app "provides-bare-symbol"
    packages { pf: "platform/main.roc" }
    imports [pf.Task Base64]
    provides main to pf
