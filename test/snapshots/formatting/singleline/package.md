# META
~~~ini
description=Singleline formatting package
type=file
~~~
# SOURCE
~~~roc
package [a!, b!] { a: "a", b: "b" }

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:1:10:1:12
EXPOSED BUT NOT DEFINED - package.md:1:14:1:16
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `a!` is exposed, but it is not defined anywhere in this module.

**package.md:1:10:1:12:**
```roc
package [a!, b!] { a: "a", b: "b" }
```
         ^^
You can fix this by either defining `a!` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `b!` is exposed, but it is not defined anywhere in this module.

**package.md:1:14:1:16:**
```roc
package [a!, b!] { a: "a", b: "b" }
```
             ^^
You can fix this by either defining `b!` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
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
			(ty-lookup (name "Str") (external-module "Str"))
			(ty-lookup (name "Str") (external-module "Str")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
