# META
~~~ini
description=Singleline with comma formatting platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires { [R1 : r1, R2 : r2,] for main : R1 -> R2 }
	exposes [E1, E2,]
	packages { pa1: "pa1", pa2: "pa2", }
	provides { pr1: "not implemented", pr2: "not implemented", }
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:5:13:5:35
EXPOSED BUT NOT DEFINED - platform.md:5:37:5:59
EXPOSED BUT NOT DEFINED - platform.md:3:11:3:13
EXPOSED BUT NOT DEFINED - platform.md:3:15:3:17
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `pr1` is exposed, but it is not defined anywhere in this module.

**platform.md:5:13:5:35:**
```roc
	provides { pr1: "not implemented", pr2: "not implemented", }
```
	           ^^^^^^^^^^^^^^^^^^^^^^
You can fix this by either defining `pr1` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `pr2` is exposed, but it is not defined anywhere in this module.

**platform.md:5:37:5:59:**
```roc
	provides { pr1: "not implemented", pr2: "not implemented", }
```
	                                   ^^^^^^^^^^^^^^^^^^^^^^
You can fix this by either defining `pr2` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `E1` is exposed, but it is not defined anywhere in this module.

**platform.md:3:11:3:13:**
```roc
	exposes [E1, E2,]
```
	         ^^
You can fix this by either defining `E1` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `E2` is exposed, but it is not defined anywhere in this module.

**platform.md:3:15:3:17:**
```roc
	exposes [E1, E2,]
```
	             ^^
You can fix this by either defining `E2` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,OpenSquare,UpperIdent,OpColon,LowerIdent,Comma,UpperIdent,OpColon,LowerIdent,Comma,CloseSquare,KwFor,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,CloseCurly,
KwExposes,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,CloseSquare,
KwPackages,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "pf")
		(requires
			(requires-entry
				(type-aliases
					(alias (name "R1") (rigid "r1"))
					(alias (name "R2") (rigid "r2")))
				(entrypoint "main")
				(ty-fn
					(ty (name "R1"))
					(ty (name "R2")))))
		(exposes
			(exposed-upper-ident (text "E1"))
			(exposed-upper-ident (text "E2")))
		(packages
			(record-field (name "pa1")
				(e-string
					(e-string-part (raw "pa1"))))
			(record-field (name "pa2")
				(e-string
					(e-string-part (raw "pa2")))))
		(provides
			(record-field (name "pr1")
				(e-string
					(e-string-part (raw "not implemented"))))
			(record-field (name "pr2")
				(e-string
					(e-string-part (raw "not implemented"))))))
	(statements))
~~~
# FORMATTED
~~~roc
platform "pf"
	requires {
		[R1 : r1, R2 : r2] for main : R1 -> R2
	}
	exposes [
		E1,
		E2,
	]
	packages {
		pa1: "pa1",
		pa2: "pa2",
	}
	provides {
		pr1: "not implemented",
		pr2: "not implemented",
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
