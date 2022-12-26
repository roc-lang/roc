app "test-missing-comma"
    packages { pf: "platform/main.roc" }
    imports [pf.Task Base64]
    provides [main, @Foo] to pf
