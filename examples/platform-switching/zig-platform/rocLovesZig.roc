app "rocLovesZig"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

# expect 
#     a = "a string so long it cannot possibly be small"  
#     b = "foo" 
#     a == b
# 
# expect 
#     a = { x: 0, y: 2 } 
#     b = { x: 1, y: 2 } 
#     a == b

expect 
    b = [456] 
    a = [ 12345, 6789, 12, 13, 14 ] 
    a == b

main = "Roc <3 Zig!\n"
