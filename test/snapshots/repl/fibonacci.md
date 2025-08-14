# META
~~~ini
description=Calculate Fibonacci number for 5
type=repl
~~~
# SOURCE
~~~roc
» fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
» fib(5)
~~~
# OUTPUT
assigned `fib`
---
Evaluation error: error.CaptureNotFound
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
~~~
# TYPES
~~~clojure
~~~
