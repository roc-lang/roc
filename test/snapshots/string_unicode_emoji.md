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
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),StringStart(2:11-2:12),StringPart(2:12-2:55),StringEnd(2:55-2:56),
LowerIdent(5:1-5:6),OpAssign(5:7-5:8),StringStart(5:9-5:10),StringPart(5:10-5:84),StringEnd(5:84-5:85),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @2.1-5.85
	(type-module @2.1-2.8)
	(statements
		(s-decl @2.1-2.56
			(p-ident @2.1-2.8 (raw "message"))
			(e-string @2.11-2.56
				(e-string-part @2.12-2.55 (raw "Hello! Here are some emojis: 👻 🎉 🚀"))))
		(s-decl @5.1-5.85
			(p-ident @5.1-5.6 (raw "greet"))
			(e-string @5.9-5.85
				(e-string-part @5.10-5.84 (raw "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.8 (ident "message"))
		(e-string @2.11-2.56
			(e-literal @2.12-2.55 (string "Hello! Here are some emojis: 👻 🎉 🚀"))))
	(d-let
		(p-assign @5.1-5.6 (ident "greet"))
		(e-string @5.9-5.85
			(e-literal @5.10-5.84 (string "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.8 (type "Str"))
		(patt @5.1-5.6 (type "Str")))
	(expressions
		(expr @2.11-2.56 (type "Str"))
		(expr @5.9-5.85 (type "Str"))))
~~~
