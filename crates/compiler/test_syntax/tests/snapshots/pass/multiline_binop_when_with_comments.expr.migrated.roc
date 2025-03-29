match
x
 + 1 # comment 1
 > 0 # comment 2
 {

    y->
    3
     * 2 # comment 3
     < 1 # comment 4

    z->
    4
     / 5 # comment 5
     < 1 # comment 6

    46 # first pattern comment
 | 95 # alternative comment 1
 | 126 # alternative comment 2
 | 150-> # This comment came after the ->
    # This comment is for the expr
    foo(bar)
    .(Result.withDefault)("") # one last comment

    _->
    42}
