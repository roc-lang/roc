# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
MODULE NOT FOUND - hello_world_with_block.md:6:1:6:17
UNUSED VARIABLE - hello_world_with_block.md:9:2:9:7
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**hello_world_with_block.md:6:1:6:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `world` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_world` to suppress this warning.
The unused variable is declared here:
**hello_world_with_block.md:9:2:9:7:**
```roc
	world = "World"
```
	^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import (raw "pf.Stdout"))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "world"))
							(e-string
								(e-string-part (raw "World"))))
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-string
								(e-string-part (raw "Hello, world!"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "world"))
					(e-string
						(e-literal (string "World"))))
				(e-call
					(e-lookup-external
						(module-idx "2")
						(target-node-idx "0"))
					(e-string
						(e-literal (string "Hello, world!")))))))
	(s-import (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> _ret")))
	(expressions
		(expr (type "_arg -> _ret"))))
~~~
