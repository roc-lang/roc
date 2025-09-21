# META
~~~ini
description=main module from package
type=package
~~~
# SOURCE
~~~roc
package [
    Color,
] {}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - main.md:2:5:2:10
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `Color` is exposed, but it is not defined anywhere in this module.

**main.md:2:5:2:10:**
```roc
    Color,
```
    ^^^^^
You can fix this by either defining `Color` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),
UpperIdent(2:5-2:10),Comma(2:10-2:11),
CloseSquare(3:1-3:2),OpenCurly(3:3-3:4),CloseCurly(3:4-3:5),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.5
	(package @1.1-3.5
		(exposes @1.9-3.2
			(exposed-upper-ident @2.5-2.10 (text "Color")))
		(packages @3.3-3.5))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[
		Color,
	]
	{}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions))
~~~
