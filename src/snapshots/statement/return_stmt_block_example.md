# META
~~~ini
description=Return statement in a block context
type=file
~~~
# SOURCE
~~~roc
module [foo]

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
INCOMPATIBLE IF BRANCHES - return_stmt_block_example.md:5:11:5:11
# PROBLEMS
**INCOMPATIBLE IF BRANCHES**
This `if` has an `else` branch with a different type from it's `then` branch:
**return_stmt_block_example.md:5:11:**
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
    _{}_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),OpArrow(3:11-3:13),UpperIdent(3:14-3:20),NoSpaceOpenRound(3:20-3:21),UpperIdent(3:21-3:24),Comma(3:24-3:25),OpenSquare(3:26-3:27),UpperIdent(3:27-3:33),CloseSquare(3:33-3:34),CloseRound(3:34-3:35),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),OpBar(4:7-4:8),LowerIdent(4:8-4:11),OpBar(4:11-4:12),OpenCurly(4:13-4:14),
LowerIdent(5:5-5:8),OpAssign(5:9-5:10),KwIf(5:11-5:13),OpenRound(5:14-5:15),LowerIdent(5:15-5:18),OpGreaterThan(5:19-5:20),Int(5:21-5:23),CloseRound(5:23-5:24),OpenCurly(5:25-5:26),
KwReturn(6:9-6:15),UpperIdent(6:16-6:19),NoSpaceOpenRound(6:19-6:20),UpperIdent(6:20-6:26),CloseRound(6:26-6:27),
CloseCurly(7:5-7:6),KwElse(7:7-7:11),OpenCurly(7:12-7:13),
StringStart(8:9-8:10),StringPart(8:10-8:15),StringEnd(8:15-8:16),
CloseCurly(9:5-9:6),
UpperIdent(10:5-10:7),NoSpaceOpenRound(10:7-10:8),LowerIdent(10:8-10:11),CloseRound(10:11-10:12),
CloseCurly(11:1-11:2),EndOfFile(11:2-11:2),
~~~
# PARSE
~~~clojure
(file @1.1-11.2
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-type-anno @3.1-3.35 (name "foo")
			(ty-fn @3.7-3.35
				(ty @3.7-3.10 (name "U64"))
				(ty-apply @3.14-3.35
					(ty @3.14-3.20 (name "Result"))
					(ty @3.21-3.24 (name "Str"))
					(ty-tag-union @3.26-3.34
						(tags
							(ty @3.27-3.33 (name "TooBig")))))))
		(s-decl @4.1-11.2
			(p-ident @4.1-4.4 (raw "foo"))
			(e-lambda @4.7-11.2
				(args
					(p-ident @4.8-4.11 (raw "num")))
				(e-block @4.13-11.2
					(statements
						(s-decl @5.5-9.6
							(p-ident @5.5-5.8 (raw "str"))
							(e-if-then-else @5.11-9.6
								(e-tuple @5.14-5.24
									(e-binop @5.15-5.23 (op ">")
										(e-ident @5.15-5.18 (raw "num"))
										(e-int @5.21-5.23 (raw "10"))))
								(e-block @5.25-7.6
									(statements
										(s-return @6.9-6.27
											(e-apply @6.16-6.27
												(e-tag @6.16-6.19 (raw "Err"))
												(e-tag @6.20-6.26 (raw "TooBig"))))))
								(e-block @7.12-9.6
									(statements
										(e-string @8.9-8.16
											(e-string-part @8.10-8.15 (raw "SMALL")))))))
						(e-apply @10.5-10.12
							(e-tag @10.5-10.7 (raw "Ok"))
							(e-ident @10.8-10.11 (raw "str")))))))))
~~~
# FORMATTED
~~~roc
module [foo]

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
		(p-assign @4.1-4.4 (ident "foo"))
		(e-closure @4.7-11.2
			(e-lambda @4.7-11.2
				(args
					(p-assign @4.8-4.11 (ident "num")))
				(e-block @4.13-11.2
					(s-let @5.5-9.6
						(p-assign @5.5-5.8 (ident "str"))
						(e-if @5.11-9.6
							(if-branches
								(if-branch
									(e-binop @5.15-5.23 (op "gt")
										(e-lookup-local @5.15-5.18
											(p-assign @4.8-4.11 (ident "num")))
										(e-int @5.21-5.23 (value "10")))
									(e-block @5.25-7.6
										(s-return @6.9-6.27
											(e-tag @6.16-6.19 (name "Err")
												(args
													(e-tag @6.20-6.26 (name "TooBig")))))
										(e-empty_record @5.25-7.6))))
							(if-else
								(e-block @7.12-9.6
									(e-string @8.9-8.16
										(e-literal @8.10-8.15 (string "SMALL")))))))
					(e-tag @10.5-10.7 (name "Ok")
						(args
							(e-lookup-local @10.8-10.11
								(p-assign @5.5-5.8 (ident "str"))))))))
		(annotation @4.1-4.4
			(declared-type
				(ty-fn @3.7-3.35 (effectful false)
					(ty @3.7-3.10 (name "U64"))
					(ty-apply @3.14-3.35 (symbol "Result")
						(ty @3.21-3.24 (name "Str"))
						(ty-tag-union @3.26-3.34
							(ty @3.27-3.33 (name "TooBig")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "U64 -> Error")))
	(expressions
		(expr @4.7-11.2 (type "U64 -> Error"))))
~~~
