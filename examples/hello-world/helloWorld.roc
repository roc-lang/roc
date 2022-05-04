app "helloWorld"
    # packages { pf: "c-platform" }
    # To switch platforms, comment-out the line above and un-comment one below.
    # packages { pf: "rust-platform" }
    # packages { pf: "swift-platform" }
    # packages { pf: "web-platform" } # See ./web-platform/README.md
    packages { pf: "zig-platform" }
    imports []
    provides [ main ] to pf

list = [ Str.concat "lllllllllllllllllllllooooooooooong" "g" ]


# currently correct
example1 = List.map list \string -> Str.repeat string 2

# currently wrong
example2 = List.map list \string -> Str.concat string "!"

main = 
    when List.get example2 0 is
        Ok s -> s
        Err _ -> "Hello, World!\n"
