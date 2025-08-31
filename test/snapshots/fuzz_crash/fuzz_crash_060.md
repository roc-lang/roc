# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]C:k||match 0{0|#
0"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColon LowerIdent OpOr KwMatch Int OpenCurly Int OpBar LineComment Int MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

C : k || match 0
#
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:2:2:3:1:**
```roc
0"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_060.md:3:1:3:2:**
```roc
}
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_060.md:3:1:3:2:**
```roc
}
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
