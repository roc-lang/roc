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
: - :0:0:0:0
# PROBLEMS
**TYPE DOES NOT SUPPORT EQUALITY**
This expression is doing an equality check on a type that does not support equality checks:
**tag_union_multiple_ineligible.md:6:8:6:24:**
```roc
expect result == result
```
       ^^^^^^^^^^^^^^^^

The type is:

    [Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others]
      where [
        b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
        b.is_gt : b, b -> Bool,
      ]

This tag union does not support equality because these tags have payload types that don't support **is_eq**:

    **Transform** (_a -> a_)
        Function equality is not supported.
    **Validate** (_a -> Bool
  where [
    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
    a.is_gt : a, a -> Bool,
  ]_)
        Function equality is not supported.
**Hint:** Tag unions only have an **is_eq** method if all of their payload types have **is_eq** methods.


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
					(e-binop (op "gt")
						(e-lookup-local
							(p-assign (ident "n")))
						(e-num (value "0")))))))
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
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-lookup-local
				(p-assign (ident "result"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(patt (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]")))
	(expressions
		(expr (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))
		(expr (type "[Ok(Str), Transform(a -> a), Validate(b -> Bool), Err(Str), .._others] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.is_gt : b, b -> Bool]"))))
~~~
