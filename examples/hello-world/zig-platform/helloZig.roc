app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf


main = 
    a = 'A'
    b = 'C'

    (
        expect a == b
        "Hello, World!\n"
    )
