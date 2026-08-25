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
── ✗ missing nested type ───────────────────── can_import_nested_mods.md:6:15

Config is in scope, but it doesn't have a nested type named Settings.

parseConfig : Config.Settings -> Str
              ^^^^^^^^^^^^^^^

── ✗ does not exist ────────────────────────── can_import_nested_mods.md:7:26

Config.toString does not exist.

parseConfig = |settings| Config.toString(settings)
                         ^^^^^^^^^^^^^^^

Config is in scope, but it has no associated toString.

── ✗ missing nested type ──────────────────── can_import_nested_mods.md:10:28

HttpAuth is in scope, but it doesn't have a nested type named Token.

authenticate : Str, Str -> HttpAuth.Token
                           ^^^^^^^^^^^^^^

── ✗ does not exist ───────────────────────── can_import_nested_mods.md:11:29

HttpAuth.login does not exist.

authenticate = |user, pass| HttpAuth.login(user, pass)
                            ^^^^^^^^^^^^^^

HttpAuth is in scope, but it has no associated login.

── ✗ missing nested type ──────────────────── can_import_nested_mods.md:14:15

Config is in scope, but it doesn't have a nested type named Advanced.

processData : Config.Parser.Advanced, Str -> Try(Str, Config.Parser.Error)
              ^^^^^^^^^^^^^^^^^^^^^^

── ✗ missing nested type ──────────────────── can_import_nested_mods.md:14:55

Config is in scope, but it doesn't have a nested type named Error.

processData : Config.Parser.Advanced, Str -> Try(Str, Config.Parser.Error)
                                                      ^^^^^^^^^^^^^^^^^^^

── ✗ does not exist ────────────────────────── can_import_nested_mods.md:16:5

Config.Parser.Advanced.parseWith does not exist.

Config.Parser.Advanced.parseWith(advancedConfig, input)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Config.Parser.Advanced is in scope, but it has no associated parseWith.

── ✗ name not in scope ────────────────────── can_import_nested_mods.md:20:23

Nothing is named padLeft in this scope.

formatOutput = |text| padLeft(text, Config.defaultPadding)
                      ^^^^^^^

Is it misspelled, or is there an import missing?

── ✗ does not exist ───────────────────────── can_import_nested_mods.md:20:37

Config.defaultPadding does not exist.

formatOutput = |text| padLeft(text, Config.defaultPadding)
                                    ^^^^^^^^^^^^^^^^^^^^^

Config is in scope, but it has no associated defaultPadding.

── ✗ missing nested type ──────────────────── can_import_nested_mods.md:23:16

HttpAuth is in scope, but it doesn't have a nested type named Credentials.

validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)
               ^^^^^^^^^^^^^^^^^^^^

── ✗ missing nested type ──────────────────── can_import_nested_mods.md:23:44

HttpAuth is in scope, but it doesn't have a nested type named Token.

validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)
                                           ^^^^^^^^^^^^^^

── ✗ missing nested type ──────────────────── can_import_nested_mods.md:23:60

HttpAuth is in scope, but it doesn't have a nested type named Error.

validateAuth : HttpAuth.Credentials -> Try(HttpAuth.Token, HttpAuth.Error)
                                                           ^^^^^^^^^^^^^^

── ✗ does not exist ───────────────────────── can_import_nested_mods.md:24:24

HttpAuth.validate does not exist.

validateAuth = |creds| HttpAuth.validate(creds)
                       ^^^^^^^^^^^^^^^^^

HttpAuth is in scope, but it has no associated validate.

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
