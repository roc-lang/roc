# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/11095—a single-quote literal accepts a numeric type suffix, so 'A'.U8 is the U8 65 just like 65.U8
type=repl
~~~
# SOURCE
~~~roc
» 'A'.U8
» 'A'.U8 == 65.U8
~~~
# OUTPUT
65
---
True
# PROBLEMS
NIL
