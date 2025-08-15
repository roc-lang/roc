# META
~~~ini
description=Deeply nested polymorphic function calls with multiple levels
type=repl
~~~
# SOURCE
~~~roc
» {
    identity = |x| x
    apply = |f, val| f(val)
    twice = |f, val| f(f(val))
    
    # Test deeply nested polymorphic function usage
    num1 = twice(identity, 42)
    str1 = twice(identity, "Hello")
    num2 = apply(|x| apply(identity, x), 100)
    
    { num1, num2, str1 }
}
~~~
# OUTPUT
Evaluation error: error.ZeroSizedType
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-block @1.2-1.3
	(e-empty_record @1.2-1.3))
~~~
# TYPES
~~~clojure
(expr @1.2-1.3 (type "{}"))
~~~
