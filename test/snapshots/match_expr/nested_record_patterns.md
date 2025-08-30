# META
~~~ini
description=Match expression with deeply nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, address: { city, country } } => "${name} lives in ${city}, ${country}"
    { person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
    { data: { info: { value } } } => "Deep nested: ${value}"
    { simple } => "Simple: ${simple}"
    {} => "empty"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent CloseCurly OpFatArrow String OpenCurly CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (ellipsis)
)
  (branch1     (binop_thick_arrow
      (record_literal
        (lc "name")
        (binop_colon
          (lc "address")
          (record_literal
            (lc "city")
            (lc "country")
          )
        )
      )
      (str_literal_big "${name} lives in ${city}, ${country}")
    )
)
  (branch2     (binop_thick_arrow
      (record_literal
        (binop_colon
          (lc "person")
          (record_literal
            (lc "name")
            (lc "age")
          )
        )
        (binop_colon
          (lc "location")
          (block
            (lc "city")
          )
        )
      )
      (str_literal_big "${name} (${age.to_str()}) from ${city}")
    )
)
  (branch3     (binop_thick_arrow
      (block
        (binop_colon
          (lc "data")
          (block
            (binop_colon
              (lc "info")
              (block
                (lc "value")
              )
            )
          )
        )
      )
      (str_literal_big "Deep nested: ${value}")
    )
)
  (branch4     (binop_thick_arrow
      (block
        (lc "simple")
      )
      (str_literal_big "Simple: ${simple}")
    )
)
  (branch5     (binop_thick_arrow
      (record_literal)
      (str_literal_big "empty")
    )
))
~~~
# FORMATTED
~~~roc
match ...
	{name, address: {city, country}} => "${name} lives in ${city}, ${country}"
	{person: {name, age}, location: {
		city
	}} => "${name} (${age.to_str()}) from ${city}"
	{
		data : {
			info : {
				value
			}
		}
	} => "Deep nested: ${value}"
	{
		simple
	} => "Simple: ${simple}"
	{} => "empty"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:42 to 2:44

**Unsupported Node**
at 3:5 to 3:50

**Unsupported Node**
at 4:35 to 4:37

**Unsupported Node**
at 5:5 to 5:15

**Unsupported Node**
at 6:8 to 6:10

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
