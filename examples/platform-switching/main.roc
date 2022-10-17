app "rocLovesPlatforms"
    packages { pf: "c-platform/main.roc" }
    # To switch platforms, comment-out the line above and un-comment one below.
    # packages { pf: "rust-platform/main.roc" }
    # packages { pf: "swift-platform/main.roc" }
    # packages { pf: "web-assembly-platform/main.roc" } # See ./web-assembly-platform/README.md
    # packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

main = "Which platform am I running on now?\n"
