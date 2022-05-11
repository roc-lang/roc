app "helloZig"
    packages { pf: "." }
    imports []
    provides [ main ] to pf

Id a := a

f : Id a -> a 
f = \@Id x -> x 

main = f (@Id "Hello, World!\n")
