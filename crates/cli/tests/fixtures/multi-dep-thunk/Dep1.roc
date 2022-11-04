interface Dep1 exposes [value1] imports [Dep2]

value1 : {} -> Str
value1 = \_ -> Dep2.value2 {}
