# META
~~~ini
description=Singleline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [a!, b!]

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:1:9:1:11
EXPOSED BUT NOT DEFINED - hosted.md:1:13:1:15
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `a!` is exposed, but it is not defined anywhere in this module.

**hosted.md:1:9:1:11:**
```roc
hosted [a!, b!]
```
        ^^
You can fix this by either defining `a!` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `b!` is exposed, but it is not defined anywhere in this module.

**hosted.md:1:13:1:15:**
```roc
hosted [a!, b!]
```
            ^^
You can fix this by either defining `b!` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwHosted,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(hosted
		(exposes
			(exposed-lower-ident
				(text "a!"))
			(exposed-lower-ident
				(text "b!"))))
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
