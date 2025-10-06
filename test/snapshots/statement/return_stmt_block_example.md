# META
~~~ini
description=Return statement in a block context
type=snippet
~~~
# SOURCE
~~~roc
foo : U64 -> Result(Str, [TooBig])
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
INCOMPATIBLE IF BRANCHES - return_stmt_block_example.md:3:11:3:11
# PROBLEMS
**INCOMPATIBLE IF BRANCHES**
This `if` has an `else` branch with a different type from it's `then` branch:
**return_stmt_block_example.md:3:11:**
```roc
    str = if (num > 10) {
        return Err(TooBig)
    } else {
        "SMALL"
    }
```
        ^^^^^^^

The `else` branch has the type:
    _Str_

But the `then` branch has the type:
    _Result(ok, [TooBig]_others)_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),OpArrow(1:11-1:13),UpperIdent(1:14-1:20),NoSpaceOpenRound(1:20-1:21),UpperIdent(1:21-1:24),Comma(1:24-1:25),OpenSquare(1:26-1:27),UpperIdent(1:27-1:33),CloseSquare(1:33-1:34),CloseRound(1:34-1:35),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),OpBar(2:7-2:8),LowerIdent(2:8-2:11),OpBar(2:11-2:12),OpenCurly(2:13-2:14),
LowerIdent(3:5-3:8),OpAssign(3:9-3:10),KwIf(3:11-3:13),OpenRound(3:14-3:15),LowerIdent(3:15-3:18),OpGreaterThan(3:19-3:20),Int(3:21-3:23),CloseRound(3:23-3:24),OpenCurly(3:25-3:26),
KwReturn(4:9-4:15),UpperIdent(4:16-4:19),NoSpaceOpenRound(4:19-4:20),UpperIdent(4:20-4:26),CloseRound(4:26-4:27),
CloseCurly(5:5-5:6),KwElse(5:7-5:11),OpenCurly(5:12-5:13),
StringStart(6:9-6:10),StringPart(6:10-6:15),StringEnd(6:15-6:16),
CloseCurly(7:5-7:6),
UpperIdent(8:5-8:7),NoSpaceOpenRound(8:7-8:8),LowerIdent(8:8-8:11),CloseRound(8:11-8:12),
CloseCurly(9:1-9:2),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.2
	(type-module @1.1-1.4)
	(statements
		(s-type-anno @1.1-1.35 (name "foo")
			(ty-fn @1.7-1.35
				(ty @1.7-1.10 (name "U64"))
				(ty-apply @1.14-1.35
					(ty @1.14-1.20 (name "Result"))
					(ty @1.21-1.24 (name "Str"))
					(ty-tag-union @1.26-1.34
						(tags
							(ty @1.27-1.33 (name "TooBig")))))))
		(s-decl @2.1-9.2
			(p-ident @2.1-2.4 (raw "foo"))
			(e-lambda @2.7-9.2
				(args
					(p-ident @2.8-2.11 (raw "num")))
				(e-block @2.13-9.2
					(statements
						(s-decl @3.5-7.6
							(p-ident @3.5-3.8 (raw "str"))
							(e-if-then-else @3.11-7.6
								(e-tuple @3.14-3.24
									(e-binop @3.15-3.23 (op ">")
										(e-ident @3.15-3.18 (raw "num"))
										(e-int @3.21-3.23 (raw "10"))))
								(e-block @3.25-5.6
									(statements
										(s-return @4.9-4.27
											(e-apply @4.16-4.27
												(e-tag @4.16-4.19 (raw "Err"))
												(e-tag @4.20-4.26 (raw "TooBig"))))))
								(e-block @5.12-7.6
									(statements
										(e-string @6.9-6.16
											(e-string-part @6.10-6.15 (raw "SMALL")))))))
						(e-apply @8.5-8.12
							(e-tag @8.5-8.7 (raw "Ok"))
							(e-ident @8.8-8.11 (raw "str")))))))))
~~~
# FORMATTED
~~~roc
foo : U64 -> Result(Str, [TooBig])
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
		(p-assign @2.1-2.4 (ident "foo"))
		(e-lambda @2.7-9.2
			(args
				(p-assign @2.8-2.11 (ident "num")))
			(e-block @2.13-9.2
				(s-let @3.5-7.6
					(p-assign @3.5-3.8 (ident "str"))
					(e-if @3.11-7.6
						(if-branches
							(if-branch
								(e-binop @3.15-3.23 (op "gt")
									(e-lookup-local @3.15-3.18
										(p-assign @2.8-2.11 (ident "num")))
									(e-num @3.21-3.23 (value "10")))
								(e-block @3.25-5.6
									(e-nominal @4.16-4.27 (nominal "Result")
										(e-tag @4.16-4.27 (name "Err")
											(args
												(e-tag @4.20-4.26 (name "TooBig"))))))))
						(if-else
							(e-block @5.12-7.6
								(e-string @6.9-6.16
									(e-literal @6.10-6.15 (string "SMALL")))))))
				(e-nominal @8.5-8.12 (nominal "Result")
					(e-tag @8.5-8.12 (name "Ok")
						(args
							(e-lookup-local @8.8-8.11
								(p-assign @3.5-3.8 (ident "str"))))))))
		(annotation @2.1-2.4
			(declared-type
				(ty-fn @1.7-1.35 (effectful false)
					(ty-lookup @1.7-1.10 (name "U64") (builtin))
					(ty-apply @1.14-1.35 (name "Result") (local)
						(ty-lookup @1.14-1.35 (name "Str") (builtin))
						(ty-tag-union @1.14-1.35
							(ty-tag-name @1.27-1.33 (name "TooBig")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.4 (type "Num(Int(Unsigned64)) -> Result(Error, [TooBig])")))
	(expressions
		(expr @2.7-9.2 (type "Num(Int(Unsigned64)) -> Result(Error, [TooBig])"))))
~~~
