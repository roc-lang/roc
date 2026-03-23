# META
~~~ini
description=Multiline string with 7 lines split - memory leak test
type=repl
~~~
# SOURCE
~~~roc
» input = "L68\nL30\nR48\nL5\nR60\nL55\nL1"
» input.split_on("\n")
~~~
# PROBLEMS
NIL
