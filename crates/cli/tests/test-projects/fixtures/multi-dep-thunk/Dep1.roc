module [value1]

import Dep2

value1 : {} -> Str
value1 = \_ -> Dep2.value2({})
