app "helloZig"
    packages { pf: "." }
    imports []
    provides [main] to pf

x = 42

main = 
    expect x != x
    "Hello, World!\n"
