# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform"",r:"
}
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String Comma LowerIdent OpColon MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app
{
	f: "" platform [],
}"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPECTED STRING**
A parsing error occurred: **expected_package_or_platform_string**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_058.md:1:1:1:22:**
```roc
app[]{f:platform"",r:"
```
^^^^^^^^^^^^^^^^^^^^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_package_platform_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_058.md:1:1:1:22:**
```roc
app[]{f:platform"",r:"
```
^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_058.md:1:22:2:1:**
```roc
app[]{f:platform"",r:"
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_058.md:2:1:2:2:**
```roc
}
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_058.md:1:22:2:1:**
```roc
app[]{f:platform"",r:"
}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_058.md:2:1:2:2:**
```roc
}
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
