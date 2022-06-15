app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = 
    a = Str.concat "a" "foo" 
    b = Str.concat "1111111111111111111111111111111111111111111111a" "bar" 

    (
        expect a == b
        "Hello, World!\n"
    )
