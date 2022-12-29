app "helloWorld"
    packages { pf: "platform-switching/c-platform/main.roc" }
    imports []
    provides [main] to pf

main = "Hello, World!\n"
