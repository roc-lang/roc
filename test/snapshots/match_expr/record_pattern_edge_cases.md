# META
~~~ini
description=Edge cases for nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { a: { b: { c } } } => "deeply nested: ${c}"
    { x, y: {} } => "mixed with empty: ${x}"
    { outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
    { a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
    { name: x } => "renamed: ${x}"
    { person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
    {} => "empty record"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly Comma LowerIdent CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon LowerIdent CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (ellipsis)
)
  (branch1     (binop_thick_arrow
      (record_literal
        (binop_colon
          (lc "a")
          (record_literal
            (binop_colon
              (lc "b")
              (record_literal
                (lc "c")
              )
            )
          )
        )
      )
      (block
        (str_literal_big "deeply nested: ${c}")
        (binop_thick_arrow
          (record_literal
            (lc "x")
            (binop_colon
              (lc "y")
              (record_literal)
            )
          )
          (str_literal_big "mixed with empty: ${x}")
        )
        (binop_thick_arrow
          (record_literal
            (binop_colon
              (lc "outer")
              (block
                (lc "inner")
              )
            )
            (lc "simple")
          )
          (str_literal_big "mixed: ${inner} and ${simple}")
        )
        (binop_thick_arrow
          (record_literal
            (binop_colon
              (lc "a")
              (block
                (lc "b")
              )
            )
            (binop_colon
              (lc "c")
              (block
                (lc "d")
              )
            )
          )
          (str_literal_big "multiple nested: ${b}, ${d}")
        )
        (binop_thick_arrow
          (block
            (binop_colon
              (lc "name")
              (lc "x")
            )
          )
          (str_literal_big "renamed: ${x}")
        )
        (binop_thick_arrow
          (block
            (binop_colon
              (lc "person")
              (record_literal
                (binop_colon
                  (lc "name")
                  (lc "firstName")
                )
                (binop_colon
                  (lc "age")
                  (lc "userAge")
                )
              )
            )
          )
          (str_literal_big "renamed nested: ${firstName} (${userAge.to_str()})")
        )
        (binop_thick_arrow
          (record_literal)
          (str_literal_big "empty record")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match ...
	{a: {b: {c}}} => 
		"deeply nested: ${c}"
		{ x, y: {} } => "mixed with empty: ${x}"
		{ outer: {
			inner
		}, simple } => "mixed: ${inner} and ${simple}"
		{ a: {
			b
		}, c: {
			d
		} } => "multiple nested: ${b}, ${d}"
		{
			name : x
		} => "renamed: ${x}"
		{
			person : {name: firstName, age: userAge}
		} => "renamed nested: ${firstName} (${userAge.to_str()})"
		{} => "empty record"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_pattern_edge_cases.md:2:5:8:25:**
```roc
    { a: { b: { c } } } => "deeply nested: ${c}"
    { x, y: {} } => "mixed with empty: ${x}"
    { outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
    { a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
    { name: x } => "renamed: ${x}"
    { person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
    {} => "empty record"
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 61
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
~~~
# TYPES
~~~roc
~~~
