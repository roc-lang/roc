# META
~~~ini
description=List.sort_with - sorting lists with custom comparison functions
type=repl
~~~
# SOURCE
~~~roc
» List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))
» List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))
» List.len(List.sort_with([], |a, b| if a < b LT else if a > b GT else EQ))
» List.len(List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ))
» List.first(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))
» List.first(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))
» List.first(List.sort_with([5, 4, 3, 2, 1], |a, b| if a > b LT else if a < b GT else EQ))
» List.len(List.sort_with([1, 1, 1, 1], |a, b| if a < b LT else if a > b GT else EQ))
» List.first(List.sort_with([1, 1, 1, 1], |a, b| if a < b LT else if a > b GT else EQ))
» List.len(List.sort_with([2, 1], |a, b| if a < b LT else if a > b GT else EQ))
» List.first(List.sort_with([2, 1], |a, b| if a < b LT else if a > b GT else EQ))
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
Ok(1)
---
Ok(1)
---
Ok(5)
---
4
---
Ok(1)
---
2
---
Ok(1)
# PROBLEMS
NIL
