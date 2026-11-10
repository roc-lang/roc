# META
~~~ini
description=List.partition_point finds the partition point via binary search
type=repl
~~~
# SOURCE
~~~roc
» [].partition_point(|x| x < 3)
» [5].partition_point(|x| x < 3)
» [5].partition_point(|x| x <= 5)
» [1, 2, 3, 4].partition_point(|x| x < 3)
» [1, 2, 3, 4, 5].partition_point(|x| x < 4)
» [1, 2, 3, 4].partition_point(|x| x < 0)
» [1, 2, 3, 4].partition_point(|x| x < 5)
» [1, 2, 2, 2, 3, 4].partition_point(|x| x <= 2)
~~~
# OUTPUT
0
---
0
---
1
---
2
---
3
---
0
---
4
---
4
# PROBLEMS
NIL
