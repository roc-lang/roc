# META
~~~ini
description=Tag union with multiple tags that have function payloads - shows which tags are ineligible
type=snippet
~~~
# SOURCE
~~~roc
x = Ok("hello")
y = Validate(|n| n > 0)
z = Transform(|s| s)
w = Err("error")
result = if True { x } else if True { y } else if True { z } else { w }
expect result == result
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - tag_union_multiple_ineligible.md:6:8:6:24
# PROBLEMS

┌────────────────────────────────┐
│ TYPE DOES NOT SUPPORT EQUALITY ├─ This expression is doing an equality ─────┐
└┬───────────────────────────────┘  check on a type that doesn't support      │
 │                                  equality.                                 │
 │                                                                            │
 │  expect result == result                                                   │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                   │
 └────────────────────────────────────── tag_union_multiple_ineligible.md:6:8 ┘

    The type is:

        [Err(a), Ok(b), Transform(c -> c), Validate(d -> Bool), ..]
          where [
            a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),
            b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]),
            d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
            d.is_gt : d, d -> Bool,
          ]

    This tag union does not support equality because these tags have payload
    types that don't support is_eq:

        Transform (a -> a)
            Function equality is not supported.
        Validate (a -> Bool
      where [
        a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
        a.is_gt : a, a -> Bool,
      ])
            Function equality is not supported.
    Hint: Tag unions only have an is_eq method if all of their payload types
    have is_eq methods.

# TOKENS
~~~zig
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpGreaterThan,Int,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,KwIf,UpperIdent,OpenCurly,LowerIdent,CloseCurly,KwElse,KwIf,UpperIdent,OpenCurly,LowerIdent,CloseCurly,KwElse,KwIf,UpperIdent,OpenCurly,LowerIdent,CloseCurly,KwElse,OpenCurly,LowerIdent,CloseCurly,
KwExpect,LowerIdent,OpEquals,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-tag (raw "Ok"))
				(e-string
					(e-string-part (raw "hello")))))
		(s-decl
			(p-ident (raw "y"))
			(e-apply
				(e-tag (raw "Validate"))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-binop (op ">")
						(e-ident (raw "n"))
						(e-int (raw "0"))))))
		(s-decl
			(p-ident (raw "z"))
			(e-apply
				(e-tag (raw "Transform"))
				(e-lambda
					(args
						(p-ident (raw "s")))
					(e-ident (raw "s")))))
		(s-decl
			(p-ident (raw "w"))
			(e-apply
				(e-tag (raw "Err"))
				(e-string
					(e-string-part (raw "error")))))
		(s-decl
			(p-ident (raw "result"))
			(e-if-then-else
				(e-tag (raw "True"))
				(e-block
					(statements
						(e-ident (raw "x"))))
				(e-if-then-else
					(e-tag (raw "True"))
					(e-block
						(statements
							(e-ident (raw "y"))))
					(e-if-then-else
						(e-tag (raw "True"))
						(e-block
							(statements
								(e-ident (raw "z"))))
						(e-block
							(statements
								(e-ident (raw "w"))))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-ident (raw "result"))))))
~~~
# FORMATTED
~~~roc
x = Ok("hello")

y = Validate(|n| n > 0)

z = Transform(|s| s)

w = Err("error")

result = if True {
	x
} else if True {
	y
} else if True {
	z
} else {
	w
}
expect result == result
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-tag (name "Ok")
			(args
				(e-string
					(e-literal (string "hello"))))))
	(d-let
		(p-assign (ident "y"))
		(e-tag (name "Validate")
			(args
				(e-lambda
					(args
						(p-assign (ident "n")))
					(e-dispatch-call (method "is_gt") (constraint-fn-var 104)
						(receiver
							(e-lookup-local
								(p-assign (ident "n"))))
						(args
							(e-num (value "0"))))))))
	(d-let
		(p-assign (ident "z"))
		(e-tag (name "Transform")
			(args
				(e-lambda
					(args
						(p-assign (ident "s")))
					(e-lookup-local
						(p-assign (ident "s")))))))
	(d-let
		(p-assign (ident "w"))
		(e-tag (name "Err")
			(args
				(e-string
					(e-literal (string "error"))))))
	(d-let
		(p-assign (ident "result"))
		(e-if
			(if-branches
				(if-branch
					(e-tag (name "True"))
					(e-block
						(e-lookup-local
							(p-assign (ident "x")))))
				(if-branch
					(e-tag (name "True"))
					(e-block
						(e-lookup-local
							(p-assign (ident "y")))))
				(if-branch
					(e-tag (name "True"))
					(e-block
						(e-lookup-local
							(p-assign (ident "z"))))))
			(if-else
				(e-block
					(e-lookup-local
						(p-assign (ident "w")))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "result"))))
			(rhs
				(e-lookup-local
					(p-assign (ident "result")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]")))
	(expressions
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Err(Str), Ok(Str), Transform(a -> a), Validate(b -> Bool), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))))
~~~
