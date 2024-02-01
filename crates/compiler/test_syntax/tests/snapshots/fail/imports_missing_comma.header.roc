app "test-missing-comma"
    packages { pf: "platform/main.roc" }
    provides [main, @Foo] to pf
