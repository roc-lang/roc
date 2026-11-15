BoxyDemandedOverrides :: [].{}

import BoxyDemandedOverridesDep

# Every `Token` is equal to every other, hashes the same, and prints as
# `custom-token`, so a walker that used structural equality, hashing, or
# printing instead of an override gives a different result. Each expectation
# below reaches an override only through the path its comment names.
Token := [Token(U64)].{
	is_eq : Token, Token -> Bool
	is_eq = |_, _| Bool.True

	to_hash : Token, Hasher -> Hasher
	to_hash = |_, hasher| "custom-token".to_hash(hasher)

	to_inspect : Token -> Str
	to_inspect = |_| "custom-token"
}

# A box payload.
expect Str.inspect(Box.box(Token.Token(1))).contains("custom-token")
expect Str.inspect({ boxed: Box.box(Token.Token(1)) }).contains("custom-token")

# A type table an erased callable captures.
show_later : a -> ({} -> Str)
show_later = |value| |{}| Str.inspect(value)

expect show_later(Token.Token(1))({}) == "custom-token"

# A list whose items are dynamic values of a generic body.
inspect_all : List(a) -> Str
inspect_all = |items| Str.inspect(items)

expect inspect_all([Token.Token(1), Token.Token(2)]) == "[custom-token, custom-token]"

# Workers of the Builtin unit.
expect List.contains([Token.Token(1)], Token.Token(2))
expect Dict.single(Token.Token(1), 5.U64).contains(Token.Token(2))

# A generic worker of another module, whose table this module fills.
expect BoxyDemandedOverridesDep.render(Token.Token(1)) == "custom-token"
expect BoxyDemandedOverridesDep.same(Token.Token(1), Token.Token(2))

# A function value compile-time evaluation stores, restored at run time.
stored_show : {} -> Str
stored_show = show_later(Token.Token(3))

expect stored_show({}) == "custom-token"

# A value compile-time evaluation prints.
evaluated : Str
evaluated = Str.inspect([Token.Token(4)])

expect evaluated == "[custom-token]"

# A record field, a tag payload of a recursive nominal type, and a nominal
# type whose equality and hashing are derived from its backing.
Tree := [Leaf, Node(Tree, Token)].{
	is_eq : _
}

Wrapper := [Wrapper(Token)].{
	is_eq : _
	to_hash : _
}

expect Str.inspect({ token: Token.Token(1) }) == "{ token: custom-token }"
expect Str.inspect(Tree.Node(Tree.Node(Tree.Leaf, Token.Token(1)), Token.Token(2))).contains("custom-token")
expect Tree.Node(Tree.Leaf, Token.Token(1)) == Tree.Node(Tree.Leaf, Token.Token(2))
expect Wrapper.Wrapper(Token.Token(1)) == Wrapper.Wrapper(Token.Token(2))
expect Dict.single(Wrapper.Wrapper(Token.Token(1)), 5.U64).contains(Wrapper.Wrapper(Token.Token(2)))

# An opaque type with derived equality compares through its backing but
# prints as opaque unless it has its own `to_inspect`.
Hidden :: [Hidden(Token)].{
	is_eq : _
}

Shown :: [Shown(Token)].{
	to_inspect : Shown -> Str
	to_inspect = |_| "custom-shown"
}

expect Hidden.Hidden(Token.Token(1)) == Hidden.Hidden(Token.Token(2))
expect !Str.inspect(Hidden.Hidden(Token.Token(1))).contains("custom-token")
expect Str.inspect([Shown.Shown(Token.Token(1))]) == "[custom-shown]"
