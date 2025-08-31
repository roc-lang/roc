# META
~~~ini
description=Pattern alternatives with mixed pattern types
type=expr
~~~
# SOURCE
~~~roc
match ... {
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
	Ok(_) | Some(_) => "success value"
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
	_ => "other"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly Int OpBar Int OpBar Int OpFatArrow String String OpBar String OpFatArrow String UpperIdent OpenRound Underscore CloseRound OpBar UpperIdent OpenRound Underscore CloseRound OpFatArrow String OpenSquare CloseSquare OpBar OpenSquare Underscore CloseSquare OpFatArrow String OpenRound Int Comma Underscore CloseRound OpBar OpenRound Underscore Comma Int CloseRound OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (ellipsis)
)
  (branch1     (binop_thick_arrow
      (binop_or
        (binop_or
          (num_literal_i32 1)
          (num_literal_i32 2)
        )
        (num_literal_i32 3)
      )
      (block
        (str_literal_big "small numbers")
        (str_literal_big "hello")
        (malformed)
        (str_literal_big "greetings")
      )
    )
)
  (branch2     (binop_thick_arrow
      (binop_or
        (apply_uc
          (uc "Ok")
          (underscore)
        )
        (apply_uc
          (uc "Some")
          (underscore)
        )
      )
      (block
        (str_literal_big "success value")
        (list_literal)
        (malformed)
        (apply_anon
          (str_literal_big "short list")
          (tuple_literal
            (num_literal_i32 0)
            (underscore)
          )
        )
        (malformed)
        (str_literal_big "has zero")
      )
    )
)
  (branch3     (binop_thick_arrow
      (underscore)
      (str_literal_big "other")
    )
))
~~~
# FORMATTED
~~~roc
match ...
	(1 || 2) || 3 => 
		"small numbers"
		"hello"
		=> 
		"greetings"
	Ok(_) || Some(_) => 
		"success value"
		[]
		=> 
		"short list"((0, _))
		=> 
		"has zero"
	_ => "other"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_alternatives_mixed.md:3:20:3:23:**
```roc
	"hello" | "world" => "greetings"
```
	                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_alternatives_mixed.md:5:11:5:14:**
```roc
	[] | [_] => "short list"
```
	         ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_alternatives_mixed.md:6:18:6:21:**
```roc
	(0, _) | (_, 0) => "has zero"
```
	                ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_mixed.md:2:2:3:34:**
```roc
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_mixed.md:4:2:4:17:**
```roc
	Ok(_) | Some(_) => "success value"
```
	^^^^^^^^^^^^^^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**pattern_alternatives_mixed.md:6:6:6:7:**
```roc
	(0, _) | (_, 0) => "has zero"
```
	    ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_mixed.md:7:2:7:14:**
```roc
	_ => "other"
```
	^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
