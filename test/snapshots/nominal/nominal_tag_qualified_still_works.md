# META
~~~ini
description=Qualified syntax still works (backward compatibility)
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

myColor : Color
myColor = Color.Red

isRed : Color -> Bool
isRed = |color| match color {
    Color.Red => Bool.True
    Color.Green => Bool.False
    Color.Blue => Bool.False
}
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_qualified_still_works.md:8:18:8:27
INVALID NOMINAL TAG - nominal_tag_qualified_still_works.md:9:20:9:30
INVALID NOMINAL TAG - nominal_tag_qualified_still_works.md:10:19:10:29
# PROBLEMS
**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_qualified_still_works.md:8:18:8:27:**
```roc
    Color.Red => Bool.True
```
                 ^^^^^^^^^

The tag is:
    _True_

But the nominal type needs it to be:
    _EmptyDict_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_qualified_still_works.md:9:20:9:30:**
```roc
    Color.Green => Bool.False
```
                   ^^^^^^^^^^

The tag is:
    _False_

But the nominal type needs it to be:
    _EmptyDict_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_qualified_still_works.md:10:19:10:29:**
```roc
    Color.Blue => Bool.False
```
                  ^^^^^^^^^^

The tag is:
    _False_

But the nominal type needs it to be:
    _EmptyDict_

# TOKENS
~~~zig
UpperIdent(1:1-1:6),OpColonEqual(1:7-1:9),OpenSquare(1:10-1:11),UpperIdent(1:11-1:14),Comma(1:14-1:15),UpperIdent(1:16-1:21),Comma(1:21-1:22),UpperIdent(1:23-1:27),CloseSquare(1:27-1:28),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),UpperIdent(3:11-3:16),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),UpperIdent(4:11-4:16),NoSpaceDotUpperIdent(4:16-4:20),
LowerIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:14),OpArrow(6:15-6:17),UpperIdent(6:18-6:22),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),OpBar(7:9-7:10),LowerIdent(7:10-7:15),OpBar(7:15-7:16),KwMatch(7:17-7:22),LowerIdent(7:23-7:28),OpenCurly(7:29-7:30),
UpperIdent(8:5-8:10),NoSpaceDotUpperIdent(8:10-8:14),OpFatArrow(8:15-8:17),UpperIdent(8:18-8:22),NoSpaceDotUpperIdent(8:22-8:27),
UpperIdent(9:5-9:10),NoSpaceDotUpperIdent(9:10-9:16),OpFatArrow(9:17-9:19),UpperIdent(9:20-9:24),NoSpaceDotUpperIdent(9:24-9:30),
UpperIdent(10:5-10:10),NoSpaceDotUpperIdent(10:10-10:15),OpFatArrow(10:16-10:18),UpperIdent(10:19-10:23),NoSpaceDotUpperIdent(10:23-10:29),
CloseCurly(11:1-11:2),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.2
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
		(s-type-anno @3.1-3.16 (name "myColor")
			(ty @3.11-3.16 (name "Color")))
		(s-decl @4.1-4.20
			(p-ident @4.1-4.8 (raw "myColor"))
			(e-tag @4.11-4.20 (raw "Color.Red")))
		(s-type-anno @6.1-6.22 (name "isRed")
			(ty-fn @6.9-6.22
				(ty @6.9-6.14 (name "Color"))
				(ty @6.18-6.22 (name "Bool"))))
		(s-decl @7.1-11.2
			(p-ident @7.1-7.6 (raw "isRed"))
			(e-lambda @7.9-11.2
				(args
					(p-ident @7.10-7.15 (raw "color")))
				(e-match
					(e-ident @7.23-7.28 (raw "color"))
					(branches
						(branch @8.5-8.27
							(p-tag @8.5-8.14 (raw ".Red"))
							(e-tag @8.18-8.27 (raw "Bool.True")))
						(branch @9.5-9.30
							(p-tag @9.5-9.16 (raw ".Green"))
							(e-tag @9.20-9.30 (raw "Bool.False")))
						(branch @10.5-10.29
							(p-tag @10.5-10.15 (raw ".Blue"))
							(e-tag @10.19-10.29 (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
Color := [Red, Green, Blue]

myColor : Color
myColor = Color.Red

isRed : Color -> Bool
isRed = |color| match color {
	Color.Red => Bool.True
	Color.Green => Bool.False
	Color.Blue => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "myColor"))
		(e-nominal @4.11-4.20 (nominal "Color")
			(e-tag @4.11-4.20 (name "Red")))
		(annotation @4.1-4.8
			(declared-type
				(ty-lookup @3.11-3.16 (name "Color") (local)))))
	(d-let
		(p-assign @7.1-7.6 (ident "isRed"))
		(e-lambda @7.9-11.2
			(args
				(p-assign @7.10-7.15 (ident "color")))
			(e-match @7.17-11.2
				(match @7.17-11.2
					(cond
						(e-lookup-local @7.23-7.28
							(p-assign @7.10-7.15 (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @8.5-8.14
										(p-applied-tag @8.5-8.14))))
							(value
								(e-nominal-external @8.18-8.27
									(module-idx "2")
									(target-node-idx "1")
									(e-tag @8.18-8.27 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @9.5-9.16
										(p-applied-tag @9.5-9.16))))
							(value
								(e-nominal-external @9.20-9.30
									(module-idx "2")
									(target-node-idx "1")
									(e-tag @9.20-9.30 (name "False")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @10.5-10.15
										(p-applied-tag @10.5-10.15))))
							(value
								(e-nominal-external @10.19-10.29
									(module-idx "2")
									(target-node-idx "1")
									(e-tag @10.19-10.29 (name "False")))))))))
		(annotation @7.1-7.6
			(declared-type
				(ty-fn @6.9-6.22 (effectful false)
					(ty-lookup @6.9-6.14 (name "Color") (local))
					(ty-lookup @6.18-6.22 (name "Bool") (external (module-idx "2") (target-node-idx "1")))))))
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
		(patt @4.1-4.8 (type "Color"))
		(patt @7.1-7.6 (type "Color -> Error")))
	(type_decls
		(nominal @1.1-1.28 (type "Color")
			(ty-header @1.1-1.6 (name "Color"))))
	(expressions
		(expr @4.11-4.20 (type "Color"))
		(expr @7.9-11.2 (type "Color -> Error"))))
~~~
