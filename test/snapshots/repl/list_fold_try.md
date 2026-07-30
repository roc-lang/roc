# META
~~~ini
description=List.fold_try folds until the first Err
type=repl
~~~
# SOURCE
~~~roc
» [1, 2, 3].fold_try(0, |sum, n| Ok(sum + n))
» [1, 2, 3, 4].fold_try(0, |sum, n| if n < 3 { Ok(sum + n) } else { Err(Stop) })
» [1.I64, 2, 3].fold_try!(0, |sum, n| Ok(sum + n))
» List.fold_try([], 0, |sum, n| Ok(sum + n))
~~~
# OUTPUT
Ok(6.0)
---
Err(Stop)
---
Ok(6)
---
Ok(0.0)
# PROBLEMS
NIL
