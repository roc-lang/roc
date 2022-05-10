app "helloZig"
    packages { pf: "." }
    imports []
    provides [ main ] to pf

f : Str -> Str
f = \s -> Str.concat s "!"

main = f "Hello, World!\n"
