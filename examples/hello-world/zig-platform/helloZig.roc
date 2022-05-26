app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf


main = 
    a = 'X'
    b = 'y'

    (
        expect a == b
        "Hello, World!\n"
    )
