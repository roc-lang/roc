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
      (block
        (str_literal_big "${name} lives in ${city}, ${country}")
        (binop_thick_arrow
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
        (binop_thick_arrow
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
        (binop_thick_arrow
          (block
            (lc "simple")
          )
          (str_literal_big "Simple: ${simple}")
        )
        (binop_thick_arrow
          (record_literal)
          (str_literal_big "empty")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match ...
	{name, address: {city, country}} => 
		"${name} lives in ${city}, ${country}"
		{ person: { name, age }, location: {
			city
		} } => "${name} (${age.to_str()}) from ${city}"
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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nested_record_patterns.md:2:5:6:18:**
```roc
    { name, address: { city, country } } => "${name} lives in ${city}, ${country}"
    { person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
    { data: { info: { value } } } => "Deep nested: ${value}"
    { simple } => "Simple: ${simple}"
    {} => "empty"
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 42
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
~~~
# TYPES
~~~roc
~~~
