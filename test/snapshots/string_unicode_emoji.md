# META
~~~ini
description=Ensure string literals handle Unicode emojis and characters properly.
type=file
~~~
# SOURCE
~~~roc
module [message, greet]

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:23),CloseSquare(1:23-1:24),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),StringStart(4:11-4:12),StringPart(4:12-4:55),StringEnd(4:55-4:56),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),StringStart(7:9-7:10),StringPart(7:10-7:84),StringEnd(7:84-7:85),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.85
	(module @1.1-1.24
		(exposes @1.8-1.24
			(exposed-lower-ident @1.9-1.16
				(text "message"))
			(exposed-lower-ident @1.18-1.23
				(text "greet"))))
	(statements
		(s-decl @4.1-4.56
			(p-ident @4.1-4.8 (raw "message"))
			(e-string @4.11-4.56
				(e-string-part @4.12-4.55 (raw "Hello! Here are some emojis: 👻 🎉 🚀"))))
		(s-decl @7.1-7.85
			(p-ident @7.1-7.6 (raw "greet"))
			(e-string @7.9-7.85
				(e-string-part @7.10-7.84 (raw "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "message"))
		(e-string @4.11-4.56
			(e-literal @4.12-4.55 (string "Hello! Here are some emojis: 👻 🎉 🚀"))))
	(d-let
		(p-assign @7.1-7.6 (ident "greet"))
		(e-string @7.9-7.85
			(e-literal @7.10-7.84 (string "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "Str"))
		(patt @7.1-7.6 (type "Str")))
	(expressions
		(expr @4.11-4.56 (type "Str"))
		(expr @7.9-7.85 (type "Str"))))
~~~
