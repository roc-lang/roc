module [
    addAndStringify,
]

import Transitive

addAndStringify = \num1, num2 ->
    Num.toStr (Transitive.add num1 num2)

expect addAndStringify 1 2 == "3"

expect addAndStringify 3 4 == "7"
