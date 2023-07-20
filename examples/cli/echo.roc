app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Effect]
    provides [main] to pf

main = Effect.envVar "SOMETHING"
