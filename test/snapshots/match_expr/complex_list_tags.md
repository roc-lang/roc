# META
~~~ini
description=Match expression with complex list patterns containing tagged values
type=expr
~~~
# SOURCE
~~~roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
    _ => "other event pattern"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow String OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "events")
)
  (branch1     (binop_thick_arrow
      (list_literal)
      (block
        (str_literal_big "no events")
        (binop_thick_arrow
          (list_literal
            (apply_uc
              (uc "Click")
              (tuple_literal
                (lc "x")
                (lc "y")
              )
            )
          )
          (str_literal_big "single click at (${Num.toStr(x)}, ${Num.toStr(y)})")
        )
        (list_literal
          (apply_uc
            (uc "KeyPress")
            (lc "key")
          )
          (unary_double_dot <unary>)
        )
        (lc "rest")
        (binop_thick_arrow
          (malformed malformed:expr_unexpected_token)
          (str_literal_big "key ${key} pressed, ${Num.toStr(List.len(rest))} more events")
        )
        (list_literal
          (apply_uc
            (uc "Move")
            (tuple_literal
              (lc "dx")
              (lc "dy")
            )
          )
          (apply_uc
            (uc "Move")
            (tuple_literal
              (lc "dx2")
              (lc "dy2")
            )
          )
          (unary_double_dot <unary>)
        )
        (lc "others")
        (binop_thick_arrow
          (malformed malformed:expr_unexpected_token)
          (str_literal_big "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}")
        )
        (list_literal
          (apply_uc
            (uc "Scroll")
            (lc "amount")
          )
          (apply_uc
            (uc "Click")
            (tuple_literal
              (lc "x")
              (lc "y")
            )
          )
          (unary_double_dot <unary>)
        )
        (lc "remaining")
        (binop_thick_arrow
          (malformed malformed:expr_unexpected_token)
          (str_literal_big "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}")
        )
      )
    )
)
  (branch2     (binop_thick_arrow
      (underscore)
      (str_literal_big "other event pattern")
    )
))
~~~
# FORMATTED
~~~roc
match events
	[] => 
		"no events"
		[Click((x, y))] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
		[KeyPress(key), ..as ]
		rest
		] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
		[Move((dx, dy)), Move((dx2, dy2)), ..as ]
		others
		] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
		[Scroll(amount), Click((x, y)), ..as ]
		remaining
		] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
	_ => "other event pattern"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:4:24:4:27:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
                       ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**complex_list_tags.md:4:5:4:27:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
    ^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:4:31:4:33:**
```roc
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
```
                              ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:5:39:5:42:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                      ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**complex_list_tags.md:5:5:5:42:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:5:48:5:50:**
```roc
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
```
                                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:6:38:6:41:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                     ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**complex_list_tags.md:6:5:6:41:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**complex_list_tags.md:6:50:6:52:**
```roc
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```
                                                 ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**complex_list_tags.md:2:5:6:130:**
```roc
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
```


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
