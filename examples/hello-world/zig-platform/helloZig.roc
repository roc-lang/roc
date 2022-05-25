app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf

main = 
    expect 1 != 1 
    "Hello, World!\n"
