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
      (block
        (binop_colon
          (lc "a")
          (block
            (binop_colon
              (lc "b")
              (block
                (lc "c")
              )
            )
          )
        )
      )
      (str_literal_big "deeply nested: ${c}")
    )
)
  (branch2     (binop_thick_arrow
      (record_literal
        (lc "x")
        (binop_colon
          (lc "y")
          (record_literal)
        )
      )
      (str_literal_big "mixed with empty: ${x}")
    )
)
  (branch3     (binop_thick_arrow
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
)
  (branch4     (binop_thick_arrow
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
)
  (branch5     (binop_thick_arrow
      (block
        (binop_colon
          (lc "name")
          (lc "x")
        )
      )
      (str_literal_big "renamed: ${x}")
    )
)
  (branch6     (binop_thick_arrow
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
)
  (branch7     (binop_thick_arrow
      (record_literal)
      (str_literal_big "empty record")
    )
))
~~~
# FORMATTED
~~~roc
match ...
	{
		a : {
			b : {
				c
			}
		}
	} => "deeply nested: ${c}"
	{x, y: {}} => "mixed with empty: ${x}"
	{outer: {
		inner
	}, simple} => "mixed: ${inner} and ${simple}"
	{a: {
		b
	}, c: {
		d
	}} => "multiple nested: ${b}, ${d}"
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
**Unsupported Node**
at 2:25 to 2:27

**Unsupported Node**
at 3:5 to 3:17

**Unsupported Node**
at 4:34 to 4:36

**Unsupported Node**
at 5:5 to 5:27

**Unsupported Node**
at 6:17 to 6:19

**Unsupported Node**
at 7:5 to 7:50

**Unsupported Node**
at 8:8 to 8:10

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
