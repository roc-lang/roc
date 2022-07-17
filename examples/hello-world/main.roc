app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main =
    when Wrapper (Payload "err") is
        Wrapper (Payload str) -> str
        Wrapper NoPayload -> ""
