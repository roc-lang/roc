# META
~~~ini
description=List.sort - sorting lists with custom comparison functions
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.sort([3, 1, 2], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.len(List.sort([5, 2, 8, 1, 9], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.len(List.sort(List.drop_first([0], 1), |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.len(List.sort([42], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.first(List.sort([3, 1, 2], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.first(List.sort([5, 2, 8, 1, 9], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.first(List.sort([5, 4, 3, 2, 1], |a, b| if a > b FirstBeforeSecond else if a < b SecondBeforeFirst else Equivalent))
» List.len(List.sort([1, 1, 1, 1], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.first(List.sort([1, 1, 1, 1], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.len(List.sort([2, 1], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
» List.first(List.sort([2, 1], |a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent))
~~~
# OUTPUT
3
---
5
---
0
---
1
---
Ok(1.0)
---
Ok(1.0)
---
Ok(5.0)
---
4
---
Ok(1.0)
---
2
---
Ok(1.0)
# PROBLEMS
NIL
