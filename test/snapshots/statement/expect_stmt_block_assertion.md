# META
~~~ini
description=Debug expression stmt
type=snippet
~~~
# SOURCE
~~~roc
foo : Bool -> Bool
foo = |a| {
    expect a == Bool.True
    a
}
~~~
# EXPECTED
UNDECLARED TYPE - expect_stmt_block_assertion.md:1:7:1:11
UNDECLARED TYPE - expect_stmt_block_assertion.md:1:15:1:19
UNDECLARED TYPE - expect_stmt_block_assertion.md:3:17:3:21
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**expect_stmt_block_assertion.md:1:7:1:11:**
```roc
foo : Bool -> Bool
```
      ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**expect_stmt_block_assertion.md:1:15:1:19:**
```roc
foo : Bool -> Bool
```
              ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**expect_stmt_block_assertion.md:3:17:3:21:**
```roc
    expect a == Bool.True
```
                ^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwExpect,LowerIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "foo")
			(ty-fn
				(ty (name "Bool"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-block
					(statements
						(s-expect
							(e-binop (op "==")
								(e-ident (raw "a"))
								(e-tag (raw "Bool.True"))))
						(e-ident (raw "a"))))))))
~~~
# FORMATTED
~~~roc
foo : Bool -> Bool
foo = |a| {
	expect a == Bool.True
	a
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-block
				(s-expect
					(e-binop (op "eq")
						(e-lookup-local
							(p-assign (ident "a")))
						(e-runtime-error (tag "undeclared_type"))))
				(e-lookup-local
					(p-assign (ident "a")))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-malformed))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error")))
	(expressions
		(expr (type "Error -> Error"))))
~~~
