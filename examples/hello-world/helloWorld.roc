app "helloWorld"
    packages { pf: "c-platform" }
    # To switch platforms, comment-out the line above and un-comment one below.
    # packages { pf: "rust-platform" }
    # packages { pf: "swift-platform" }
    # packages { pf: "web-platform" } # See ./web-platform/README.md
    # packages { pf: "zig-platform" }
    imports []
    provides [ main ] to pf

a = 
    foobar = "Hello"

    foobar

b = 
    foobar = "World"

    foobar

# main = "Hello, World!\n"
main = Str.concat a b 
