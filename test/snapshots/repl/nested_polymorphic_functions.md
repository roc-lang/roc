# META
~~~ini
description=Nested polymorphic function calls
type=repl
~~~
# SOURCE
~~~roc
» {
    identity = |x| x
    apply = |f, val| f(val)
    
    # Test nested polymorphic function usage
    num1 = apply(identity, 10)
    str1 = apply(identity, "Test")
    num2 = apply(identity, 20)
    
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
