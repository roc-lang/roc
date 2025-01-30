module [
    add_and_stringify,
]

import Transitive

add_and_stringify = \num1, num2 ->
    Num.to_str(Transitive.add(num1, num2))

expect add_and_stringify(1, 2) == "3"

expect add_and_stringify(3, 4) == "7"
