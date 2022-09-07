app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = "Hello, World!\n"
