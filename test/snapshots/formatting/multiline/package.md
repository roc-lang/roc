# META
~~~ini
description=Multiline formatting package
type=file
~~~
# SOURCE
~~~roc
package
	[
		a!,
		b!,
	]
	{
		a: "a",
		b: "b",
	}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:3:3:3:5
EXPOSED BUT NOT DEFINED - package.md:4:3:4:5
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `a!` is exposed, but it is not defined anywhere in this module.

**package.md:3:3:3:5:**
```roc
		a!,
```
		^^
You can fix this by either defining `a!` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `b!` is exposed, but it is not defined anywhere in this module.

**package.md:4:3:4:5:**
```roc
		b!,
```
		^^
You can fix this by either defining `b!` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage,
OpenSquare,
LowerIdent,Comma,
LowerIdent,Comma,
CloseSquare,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "a!"))
			(exposed-lower-ident
				(text "b!")))
		(packages
			(record-field (name "a")
				(e-string
					(e-string-part (raw "a"))))
			(record-field (name "b")
				(e-string
					(e-string-part (raw "b"))))))
	(statements
		(s-type-anno (name "a!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-type-anno (name "b!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "a!")
		(ty-fn (effectful true)
			(ty-lookup (name "Str") (builtin))
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
