# META
~~~ini
description=multiline_string_complex
type=file
~~~
# SOURCE
~~~roc
package
	[]
	{
		x: \\Multiline
		,
	}

value1 = \\This is a "string" with just one line

value2 =
	\\This is a "string" with just one line

value3 = \\This is a string
	\\With multiple lines
	\\${value1}

value4 =
	\\This is a string
	# A comment in between
	\\With multiple lines
	\\${value2}

value5 = {
	a: \\Multiline
	,
	b: (
		\\Multiline
		,
		\\Multiline
		,
	),
	c: [
		\\multiline
		,
	],
	d: (
		0 - \\
		,
	),
	e: !\\
	,
}

x = {
	\\
	\\
}
~~~
# EXPECTED
MISSING METHOD - multiline_string_complex.md:40:5:40:8
TYPE MISMATCH - multiline_string_complex.md:37:3:37:4
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  e: !\\                                                                    │
 │     ‾‾‾                                                                    │
 └────────────────────────────────────────── multiline_string_complex.md:40:5 ┘

    The value's type, which does not have a method named `not`, is:

        Str

    Hint: For this to work, the type would need to have a method named `not`
    associated with it in the type's declaration.


┌───────────────┐
│ TYPE MISMATCH ├─ The `minus` method on `Dec` has an incompatible type. ─────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  0 - \\                                                                    │
 │  ‾                                                                         │
 └────────────────────────────────────────── multiline_string_complex.md:37:3 ┘

    The method `minus` has the type:

        Dec, Dec -> Dec

    But I need it to have the type:

        Dec, Str -> Dec

# TOKENS
~~~zig
KwPackage,
OpenSquare,CloseSquare,
OpenCurly,
LowerIdent,OpColon,MultilineStringStart,StringPart,
Comma,
CloseCurly,
LowerIdent,OpAssign,MultilineStringStart,StringPart,
LowerIdent,OpAssign,
MultilineStringStart,StringPart,
LowerIdent,OpAssign,MultilineStringStart,StringPart,
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,
LowerIdent,OpAssign,
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,MultilineStringStart,StringPart,
Comma,
LowerIdent,OpColon,OpenRound,
MultilineStringStart,StringPart,
Comma,
MultilineStringStart,StringPart,
Comma,
CloseRound,Comma,
LowerIdent,OpColon,OpenSquare,
MultilineStringStart,StringPart,
Comma,
CloseSquare,Comma,
LowerIdent,OpColon,OpenRound,
Int,OpBinaryMinus,MultilineStringStart,StringPart,
Comma,
CloseRound,Comma,
LowerIdent,OpColon,OpBang,MultilineStringStart,StringPart,
Comma,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes)
		(packages
			(record-field (name "x")
				(e-multiline-string
					(e-string-part (raw "Multiline"))))))
	(statements
		(s-decl
			(p-ident (raw "value1"))
			(e-multiline-string
				(e-string-part (raw "This is a "string" with just one line"))))
		(s-decl
			(p-ident (raw "value2"))
			(e-multiline-string
				(e-string-part (raw "This is a "string" with just one line"))))
		(s-decl
			(p-ident (raw "value3"))
			(e-multiline-string
				(e-string-part (raw "This is a string"))
				(e-string-part (raw "With multiple lines"))
				(e-string-part (raw ""))
				(e-ident (raw "value1"))
				(e-string-part (raw ""))))
		(s-decl
			(p-ident (raw "value4"))
			(e-multiline-string
				(e-string-part (raw "This is a string"))
				(e-string-part (raw "With multiple lines"))
				(e-string-part (raw ""))
				(e-ident (raw "value2"))
				(e-string-part (raw ""))))
		(s-decl
			(p-ident (raw "value5"))
			(e-record
				(field (field "a")
					(e-multiline-string
						(e-string-part (raw "Multiline"))))
				(field (field "b")
					(e-tuple
						(e-multiline-string
							(e-string-part (raw "Multiline")))
						(e-multiline-string
							(e-string-part (raw "Multiline")))))
				(field (field "c")
					(e-list
						(e-multiline-string
							(e-string-part (raw "multiline")))))
				(field (field "d")
					(e-tuple
						(e-binop (op "-")
							(e-int (raw "0"))
							(e-multiline-string
								(e-string-part (raw ""))))))
				(field (field "e")
					(unary "!"
						(e-multiline-string
							(e-string-part (raw "")))))))
		(s-decl
			(p-ident (raw "x"))
			(e-block
				(statements
					(e-multiline-string
						(e-string-part (raw ""))
						(e-string-part (raw ""))))))))
~~~
# FORMATTED
~~~roc
package
	[]
	{
		x: \\Multiline
		,
	}

value1 = \\This is a "string" with just one line

value2 = 
	\\This is a "string" with just one line

value3 = \\This is a string
	\\With multiple lines
	\\${value1}

value4 = 
	\\This is a string
	# A comment in between
	\\With multiple lines
	\\${value2}

value5 = {
	a: \\Multiline
	,
	b: (
		\\Multiline
		,
		\\Multiline
		,
	),
	c: [
		\\multiline
		,
	],
	d: (
		0 - \\
		,
	),
	e: !\\
	,
}

x = {
	\\
	\\
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value1"))
		(e-string
			(e-literal (string "This is a "string" with just one line"))))
	(d-let
		(p-assign (ident "value2"))
		(e-string
			(e-literal (string "This is a "string" with just one line"))))
	(d-let
		(p-assign (ident "value3"))
		(e-block
			(s-let
				(p-assign (ident "#interp_0"))
				(e-lookup-local
					(p-assign (ident "value1"))))
			(e-interpolation (constraint-fn-var 148)
				(first
					(e-literal (string "This is a string
With multiple lines
")))
				(parts
					(e-lookup-local
						(p-assign (ident "#interp_0")))
					(e-literal (string ""))))))
	(d-let
		(p-assign (ident "value4"))
		(e-block
			(s-let
				(p-assign (ident "#interp_1"))
				(e-lookup-local
					(p-assign (ident "value2"))))
			(e-interpolation (constraint-fn-var 204)
				(first
					(e-literal (string "This is a string
With multiple lines
")))
				(parts
					(e-lookup-local
						(p-assign (ident "#interp_1")))
					(e-literal (string ""))))))
	(d-let
		(p-assign (ident "value5"))
		(e-record
			(fields
				(field (name "a")
					(e-string
						(e-literal (string "Multiline"))))
				(field (name "b")
					(e-tuple
						(elems
							(e-string
								(e-literal (string "Multiline")))
							(e-string
								(e-literal (string "Multiline"))))))
				(field (name "c")
					(e-list
						(elems
							(e-string
								(e-literal (string "multiline"))))))
				(field (name "d")
					(e-dispatch-call (method "minus") (constraint-fn-var 318)
						(receiver
							(e-num (value "0")))
						(args
							(e-string))))
				(field (name "e")
					(e-dispatch-call (method "not") (constraint-fn-var 333)
						(receiver
							(e-string))
						(args))))))
	(d-let
		(p-assign (ident "x"))
		(e-block
			(e-string
				(e-literal (string "
"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Str"))
		(patt (type "Str"))
		(patt (type "Str"))
		(patt (type "{ a: Str, b: (Str, Str), c: List(Str), d: Error, e: Error }"))
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))
		(expr (type "Str"))
		(expr (type "Str"))
		(expr (type "Str"))
		(expr (type "{ a: Str, b: (Str, Str), c: List(Str), d: Error, e: Error }"))
		(expr (type "Str"))))
~~~
