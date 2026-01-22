# META
~~~ini
description=Test record destructuring with field rename (label differs from binding name)
type=expr
~~~
# SOURCE
~~~roc
|record| match record {
    { name, age: personAge } => personAge
    _ => 0
}
~~~
# EXPECTED
UNUSED VARIABLE - can_record_destruct_rename.md:2:7:2:11
REDUNDANT PATTERN - can_record_destruct_rename.md:1:10:4:2
# PROBLEMS
**UNUSED VARIABLE**
Variable `name` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**can_record_destruct_rename.md:2:7:2:11:**
```roc
    { name, age: personAge } => personAge
```
      ^^^^


**REDUNDANT PATTERN**
The second branch of this `match` is redundant:
**can_record_destruct_rename.md:1:10:4:2:**
```roc
|record| match record {
    { name, age: personAge } => personAge
    _ => 0
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "record")))
	(e-match
		(e-ident (raw "record"))
		(branches
			(branch
				(p-record
					(field (name "name") (rest false))
					(field (name "age") (rest false)
						(p-ident (raw "personAge"))))
				(e-ident (raw "personAge")))
			(branch
				(p-underscore)
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
|record| match record {
	{ name, age: personAge } => personAge
	_ => 0
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "record")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "record"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-record-destructure
								(destructs
									(record-destruct (label "name") (ident "name")
										(required
											(p-assign (ident "name"))))
									(record-destruct (label "age") (ident "age")
										(sub-pattern
											(p-assign (ident "personAge"))))))))
					(value
						(e-lookup-local
							(p-assign (ident "personAge")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-num (value "0"))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: a, name: _field } -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
