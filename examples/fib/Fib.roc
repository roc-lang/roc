app "fib"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

main = \n -> fib n


fib = \n ->
    if n == 0 then
        0
    else if n == 1 then
        1
    else
        (fib (n - 1)) + (fib (n - 2))

# the clever implementation requires join points
# fib = \n, a, b -> 
#     if n == 0 then
#         a
# 
#     else
#         fib (n - 1) b (a + b)
# 
# fib n 0 1
            
    
