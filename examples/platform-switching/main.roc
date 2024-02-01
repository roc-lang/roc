app "rocLovesPlatforms"
    packages { pf: "c-platform/main.roc" }
    provides [main] to pf

main = "Which platform am I running on now?\n"
