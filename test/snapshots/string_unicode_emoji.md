# META
~~~ini
description=Ensure string literals handle Unicode emojis and characters properly.
type=snippet
~~~
# SOURCE
~~~roc
# Test that Unicode emojis are properly handled in string literals
message = "Hello! Here are some emojis: 👻 🎉 🚀"

# Test other Unicode characters
greet = "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "message"))
			(e-string
				(e-string-part (raw "Hello! Here are some emojis: 👻 🎉 🚀"))))
		(s-decl
			(p-ident (raw "greet"))
			(e-string
				(e-string-part (raw "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "message"))
		(e-string
			(e-literal (string "Hello! Here are some emojis: 👻 🎉 🚀"))))
	(d-let
		(p-assign (ident "greet"))
		(e-string
			(e-literal (string "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
