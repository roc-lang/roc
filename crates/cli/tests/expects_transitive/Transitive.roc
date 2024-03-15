interface Transitive
    exposes [
        add,
    ]
    imports []

add = \num1, num2 -> (num1 + num2)

expect add 1 2 == 3
