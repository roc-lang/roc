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
		{ x, y : {} } => "mixed with empty: ${x}"
		{ outer : {
			inner
		}, simple } => "mixed: ${inner} and ${simple}"
		{ a : {
			b
		}, c : {
			d
		} } => "multiple nested: ${b}, ${d}"
		{
			name : x
		} => "renamed: ${x}"
		{
			person : {name : firstName, age : userAge}
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
(expr :tag match :type "_e")
~~~
# TYPES
~~~roc
_e
~~~
