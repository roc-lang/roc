app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf


main = 
    a = "foo" 
    b = "bar" 

    (
        expect a == b
        "Hello, World!\n"
    )
