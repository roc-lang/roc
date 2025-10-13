# META
~~~ini
description=Pattern match on nominal tag union with unqualified tags
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

isRed : Color -> Bool
isRed = |color| match color {
    Red => Bool.True
    Green => Bool.False
    Blue => Bool.False
}
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_unqualified_pattern_match.md:5:12:5:21
INVALID NOMINAL TAG - nominal_tag_unqualified_pattern_match.md:6:14:6:24
INVALID NOMINAL TAG - nominal_tag_unqualified_pattern_match.md:7:13:7:23
# PROBLEMS
**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_unqualified_pattern_match.md:5:12:5:21:**
```roc
    Red => Bool.True
```
           ^^^^^^^^^

The tag is:
    _True_

But the nominal type needs it to be:
    _EmptyDict_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_unqualified_pattern_match.md:6:14:6:24:**
```roc
    Green => Bool.False
```
             ^^^^^^^^^^

The tag is:
    _False_

But the nominal type needs it to be:
    _EmptyDict_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_unqualified_pattern_match.md:7:13:7:23:**
```roc
    Blue => Bool.False
```
            ^^^^^^^^^^

The tag is:
    _False_

But the nominal type needs it to be:
    _EmptyDict_

# TOKENS
~~~zig
UpperIdent(1:1-1:6),OpColonEqual(1:7-1:9),OpenSquare(1:10-1:11),UpperIdent(1:11-1:14),Comma(1:14-1:15),UpperIdent(1:16-1:21),Comma(1:21-1:22),UpperIdent(1:23-1:27),CloseSquare(1:27-1:28),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),UpperIdent(3:9-3:14),OpArrow(3:15-3:17),UpperIdent(3:18-3:22),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:15),OpBar(4:15-4:16),KwMatch(4:17-4:22),LowerIdent(4:23-4:28),OpenCurly(4:29-4:30),
UpperIdent(5:5-5:8),OpFatArrow(5:9-5:11),UpperIdent(5:12-5:16),NoSpaceDotUpperIdent(5:16-5:21),
UpperIdent(6:5-6:10),OpFatArrow(6:11-6:13),UpperIdent(6:14-6:18),NoSpaceDotUpperIdent(6:18-6:24),
UpperIdent(7:5-7:9),OpFatArrow(7:10-7:12),UpperIdent(7:13-7:17),NoSpaceDotUpperIdent(7:17-7:23),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.2
	(type-module @1.1-1.6)
	(statements
		(s-type-decl @1.1-1.28
			(header @1.1-1.6 (name "Color")
				(args))
			(ty-tag-union @1.10-1.28
				(tags
					(ty @1.11-1.14 (name "Red"))
					(ty @1.16-1.21 (name "Green"))
					(ty @1.23-1.27 (name "Blue")))))
		(s-type-anno @3.1-3.22 (name "isRed")
			(ty-fn @3.9-3.22
				(ty @3.9-3.14 (name "Color"))
				(ty @3.18-3.22 (name "Bool"))))
		(s-decl @4.1-8.2
			(p-ident @4.1-4.6 (raw "isRed"))
			(e-lambda @4.9-8.2
				(args
					(p-ident @4.10-4.15 (raw "color")))
				(e-match
					(e-ident @4.23-4.28 (raw "color"))
					(branches
						(branch @5.5-5.21
							(p-tag @5.5-5.8 (raw "Red"))
							(e-tag @5.12-5.21 (raw "Bool.True")))
						(branch @6.5-6.24
							(p-tag @6.5-6.10 (raw "Green"))
							(e-tag @6.14-6.24 (raw "Bool.False")))
						(branch @7.5-7.23
							(p-tag @7.5-7.9 (raw "Blue"))
							(e-tag @7.13-7.23 (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
Color := [Red, Green, Blue]

isRed : Color -> Bool
isRed = |color| match color {
	Red => Bool.True
	Green => Bool.False
	Blue => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "isRed"))
		(e-lambda @4.9-8.2
			(args
				(p-assign @4.10-4.15 (ident "color")))
			(e-match @4.17-8.2
				(match @4.17-8.2
					(cond
						(e-lookup-local @4.23-4.28
							(p-assign @4.10-4.15 (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @5.5-5.8)))
							(value
								(e-nominal-external @5.12-5.21
									(module-idx "2")
									(target-node-idx "1")
									(e-tag @5.12-5.21 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @6.5-6.10)))
							(value
								(e-nominal-external @6.14-6.24
									(module-idx "2")
									(target-node-idx "1")
									(e-tag @6.14-6.24 (name "False")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @7.5-7.9)))
							(value
								(e-nominal-external @7.13-7.23
									(module-idx "2")
									(target-node-idx "1")
									(e-tag @7.13-7.23 (name "False")))))))))
		(annotation @4.1-4.6
			(declared-type
				(ty-fn @3.9-3.22 (effectful false)
					(ty-lookup @3.9-3.14 (name "Color") (local))
					(ty-lookup @3.18-3.22 (name "Bool") (external (module-idx "2") (target-node-idx "1")))))))
	(s-nominal-decl @1.1-1.28
		(ty-header @1.1-1.6 (name "Color"))
		(ty-tag-union @1.10-1.28
			(ty-tag-name @1.11-1.14 (name "Red"))
			(ty-tag-name @1.16-1.21 (name "Green"))
			(ty-tag-name @1.23-1.27 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "Color -> Error")))
	(type_decls
		(nominal @1.1-1.28 (type "Color")
			(ty-header @1.1-1.6 (name "Color"))))
	(expressions
		(expr @4.9-8.2 (type "Color -> Error"))))
~~~
