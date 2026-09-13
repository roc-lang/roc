# META
~~~ini
description=Nested mod qualification
type=snippet
~~~
# SOURCE
~~~roc
import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config.Parser.Advanced, Str -> Try(Str, Config.Parser.Error)
processData = |advancedConfig, input|
    Config.Parser.Advanced.parseWith(advancedConfig, input)

# Test mixed qualification (exposed item + qualified)
formatOutput : Str -> Str
formatOutput = |text| padLeft(text, Config.defaultPadding)

# Test qualified type in function signature
validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)
validateAuth = |creds| HttpAuth.validate(creds)
~~~
# EXPECTED
MISSING NESTED TYPE - can_import_nested_mods.md:6:15:6:30
DOES NOT EXIST - can_import_nested_mods.md:7:26:7:41
MISSING NESTED TYPE - can_import_nested_mods.md:10:28:10:42
DOES NOT EXIST - can_import_nested_mods.md:11:29:11:43
MISSING NESTED TYPE - can_import_nested_mods.md:14:15:14:37
MISSING NESTED TYPE - can_import_nested_mods.md:14:55:14:74
DOES NOT EXIST - can_import_nested_mods.md:16:5:16:37
NAME NOT IN SCOPE - can_import_nested_mods.md:20:23:20:30
DOES NOT EXIST - can_import_nested_mods.md:20:37:20:58
MISSING NESTED TYPE - can_import_nested_mods.md:23:16:23:36
MISSING NESTED TYPE - can_import_nested_mods.md:23:44:23:58
MISSING NESTED TYPE - can_import_nested_mods.md:23:60:23:74
DOES NOT EXIST - can_import_nested_mods.md:24:24:24:41
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 6 15) (end 6 30))
		(headline
			(annotated code "Config")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Settings")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 6 15) (end 6 30) (annotation error) (line-text "parseConfig : Config.Settings -> Str"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 7 26) (end 7 41))
		(headline
			(annotated code "Config.toString")
			(reflow " does not exist."))
		(document
			(annotated code "Config")
			(reflow " is in scope, but it has no associated ")
			(annotated code "toString")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "can_import_nested_mods.md") (start 7 26) (end 7 41) (annotation error) (line-text "parseConfig = |settings| Config.toString(settings)"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 10 28) (end 10 42))
		(headline
			(annotated code "HttpAuth")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Token")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 10 28) (end 10 42) (annotation error) (line-text "authenticate : Str, Str -> HttpAuth.Token"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 11 29) (end 11 43))
		(headline
			(annotated code "HttpAuth.login")
			(reflow " does not exist."))
		(document
			(annotated code "HttpAuth")
			(reflow " is in scope, but it has no associated ")
			(annotated code "login")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "can_import_nested_mods.md") (start 11 29) (end 11 43) (annotation error) (line-text "authenticate = |user, pass| HttpAuth.login(user, pass)"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 14 15) (end 14 37))
		(headline
			(annotated code "Config")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Advanced")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 14 15) (end 14 37) (annotation error) (line-text "processData : Config.Parser.Advanced, Str -> Try(Str, Config.Parser.Error)"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 14 55) (end 14 74))
		(headline
			(annotated code "Config")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Error")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 14 55) (end 14 74) (annotation error) (line-text "processData : Config.Parser.Advanced, Str -> Try(Str, Config.Parser.Error)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 16 5) (end 16 37))
		(headline
			(annotated code "Config.Parser.Advanced.parseWith")
			(reflow " does not exist."))
		(document
			(annotated code "Config.Parser.Advanced")
			(reflow " is in scope, but it has no associated ")
			(annotated code "parseWith")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "can_import_nested_mods.md") (start 16 5) (end 16 37) (annotation error) (line-text "    Config.Parser.Advanced.parseWith(advancedConfig, input)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 20 23) (end 20 30))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "padLeft")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_nested_mods.md") (start 20 23) (end 20 30) (annotation error) (line-text "formatOutput = |text| padLeft(text, Config.defaultPadding)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 20 37) (end 20 58))
		(headline
			(annotated code "Config.defaultPadding")
			(reflow " does not exist."))
		(document
			(annotated code "Config")
			(reflow " is in scope, but it has no associated ")
			(annotated code "defaultPadding")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "can_import_nested_mods.md") (start 20 37) (end 20 58) (annotation error) (line-text "formatOutput = |text| padLeft(text, Config.defaultPadding)"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 23 16) (end 23 36))
		(headline
			(annotated code "HttpAuth")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Credentials")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 23 16) (end 23 36) (annotation error) (line-text "validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 23 44) (end 23 58))
		(headline
			(annotated code "HttpAuth")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Token")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 23 44) (end 23 58) (annotation error) (line-text "validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 23 60) (end 23 74))
		(headline
			(annotated code "HttpAuth")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Error")
			(reflow "."))
		(document
			(source-region (file "can_import_nested_mods.md") (start 23 60) (end 23 74) (annotation error) (line-text "validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 24 24) (end 24 41))
		(headline
			(annotated code "HttpAuth.validate")
			(reflow " does not exist."))
		(document
			(annotated code "HttpAuth")
			(reflow " is in scope, but it has no associated ")
			(annotated code "validate")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "can_import_nested_mods.md") (start 24 24) (end 24 41) (annotation error) (line-text "validateAuth = |creds| HttpAuth.validate(creds)")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Parser.Config"))
		(s-import (raw "http.Client.Auth") (alias "HttpAuth"))
		(s-import (raw "utils.String.Format")
			(exposing
				(exposed-lower-ident
					(text "padLeft"))))
		(s-type-anno (name "parseConfig")
			(ty-fn
				(ty (name "Config.Settings"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "parseConfig"))
			(e-lambda
				(args
					(p-ident (raw "settings")))
				(e-apply
					(e-ident (raw "Config.toString"))
					(e-ident (raw "settings")))))
		(s-type-anno (name "authenticate")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))
				(ty (name "HttpAuth.Token"))))
		(s-decl
			(p-ident (raw "authenticate"))
			(e-lambda
				(args
					(p-ident (raw "user"))
					(p-ident (raw "pass")))
				(e-apply
					(e-ident (raw "HttpAuth.login"))
					(e-ident (raw "user"))
					(e-ident (raw "pass")))))
		(s-type-anno (name "processData")
			(ty-fn
				(ty (name "Config.Parser.Advanced"))
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty (name "Config.Parser.Error")))))
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-ident (raw "advancedConfig"))
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Config.Parser.Advanced.parseWith"))
					(e-ident (raw "advancedConfig"))
					(e-ident (raw "input")))))
		(s-type-anno (name "formatOutput")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "formatOutput"))
			(e-lambda
				(args
					(p-ident (raw "text")))
				(e-apply
					(e-ident (raw "padLeft"))
					(e-ident (raw "text"))
					(e-ident (raw "Config.defaultPadding")))))
		(s-type-anno (name "validateAuth")
			(ty-fn
				(ty (name "HttpAuth.Credentials"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "HttpAuth.Token"))
					(ty (name "HttpAuth.Error")))))
		(s-decl
			(p-ident (raw "validateAuth"))
			(e-lambda
				(args
					(p-ident (raw "creds")))
				(e-apply
					(e-ident (raw "HttpAuth.validate"))
					(e-ident (raw "creds")))))))
~~~
# FORMATTED
~~~roc
import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config.Parser.Advanced, Str -> Try(Str, Config.Parser.Error)
processData = |advancedConfig, input|
	Config.Parser.Advanced.parseWith(advancedConfig, input)

# Test mixed qualification (exposed item + qualified)
formatOutput : Str -> Str
formatOutput = |text| padLeft(text, Config.defaultPadding)

# Test qualified type in function signature
validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)
validateAuth = |creds| HttpAuth.validate(creds)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parseConfig"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "authenticate"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-malformed))))
	(d-let
		(p-assign (ident "processData"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-malformed)))))
	(d-let
		(p-assign (ident "formatOutput"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "validateAuth"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(s-import (mod "json.Parser")
		(exposes
			(exposed (name "Config") (alias "Config") (wildcard false))))
	(s-import (mod "http.Client")
		(exposes
			(exposed (name "Auth") (alias "HttpAuth") (wildcard false))))
	(s-import (mod "utils.String")
		(exposes
			(exposed (name "Format") (alias "Format") (wildcard false))
			(exposed (name "Format.padLeft") (alias "padLeft") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str"))
		(patt (type "Str, Str -> Error"))
		(patt (type "Error, Str -> Try(Str, Error)"))
		(patt (type "Str -> Str"))
		(patt (type "Error -> Try(Error, Error)")))
	(expressions
		(expr (type "Error -> Str"))
		(expr (type "Str, Str -> Error"))
		(expr (type "Error, Str -> Try(Str, Error)"))
		(expr (type "Str -> Str"))
		(expr (type "Error -> Try(Error, Error)"))))
~~~
