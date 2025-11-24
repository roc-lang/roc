# META
~~~ini
description=Return statement in a block context
type=snippet
~~~
# SOURCE
~~~roc
foo : U64 -> Try(Str, [TooBig])
foo = |num| {
    str = if (num > 10) {
        return Err(TooBig)
    } else {
        "SMALL"
    }
    Ok(str)
}
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - return_stmt_block_example.md:6:10:6:15
TYPE MISMATCH - return_stmt_block_example.md:8:5:8:12
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `try_from_str` on a type that doesn't support methods:
**return_stmt_block_example.md:6:10:6:15:**
```roc
        "SMALL"
```
         ^^^^^

This type doesn't support methods:
    _[Err([TooBig]_others)]_others2_



**TYPE MISMATCH**
This expression is used in an unexpected way:
**return_stmt_block_example.md:8:5:8:12:**
```roc
    Ok(str)
```
    ^^^^^^^

It has the type:
    _[Ok([Err([TooBig]_others)][ProvidedByCompiler])][Err([TooBig])]_

But the type annotation says it should have the type:
    _Try(Str, [TooBig])_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwIf,OpenRound,LowerIdent,OpGreaterThan,Int,CloseRound,OpenCurly,
KwReturn,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,KwElse,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
				(ty (name "U64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "TooBig")))))))
		(s-decl
			(p-ident (raw "foo"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "str"))
							(e-if-then-else
								(e-tuple
									(e-binop (op ">")
										(e-ident (raw "num"))
										(e-int (raw "10"))))
								(e-block
									(statements
										(s-return
											(e-apply
												(e-tag (raw "Err"))
												(e-tag (raw "TooBig"))))))
								(e-block
									(statements
										(e-string
											(e-string-part (raw "SMALL")))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "str")))))))))
~~~
# FORMATTED
~~~roc
foo : U64 -> Try(Str, [TooBig])
foo = |num| {
	str = if (num > 10) {
		return Err(TooBig)
	} else {
		"SMALL"
	}
	Ok(str)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-block
				(s-let
					(p-assign (ident "str"))
					(e-if
						(if-branches
							(if-branch
								(e-binop (op "gt")
									(e-lookup-local
										(p-assign (ident "num")))
									(e-num (value "10")))
								(e-block
									(e-tag (name "Err")
										(args
											(e-tag (name "TooBig")))))))
						(if-else
							(e-block
								(e-string
									(e-literal (string "SMALL")))))))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "str")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "TooBig"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> Error")))
	(expressions
		(expr (type "U64 -> Error"))))
~~~
