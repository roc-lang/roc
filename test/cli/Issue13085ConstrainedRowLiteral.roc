# https://github.com/roc-lang/roc/issues/13085
# A closed literal conversion does not need the enclosing record-row variable.
Issue13085ConstrainedRowLiteral := [].{
	make : { id : Str, ..new } -> U64 where [new.describe : new -> Str]
	make = |whole| {
		built = |x| { count: 1, got: x }
		built(whole).count
	}

	make_captured : { id : Str, ..new } -> U64 where [new.describe : new -> Str]
	make_captured = |whole| {
		built = |x| { count: 1, got: x, name: whole.id }
		built(whole).count
	}

	make_aliased : { id : Str, ..new } -> U64 where [new.describe : new -> Str]
	make_aliased = |whole| {
		built = |x| { count: 1, got: x }
		alias = built
		alias(whole).count
	}

	make_quote : { id : Str, ..new } -> Str where [new.describe : new -> Str]
	make_quote = |whole| {
		built = |x| { label: "ok", got: x }
		built(whole).label
	}

	make_custom : { id : Str, ..new } -> U64 where [new.describe : new -> Str]
	make_custom = |whole| {
		Count := { value : U64 }.{
			from_numeral : Numeral -> Try(Count, [InvalidNumeral(Str)])
			from_numeral = |_| Ok({ value: 1 })
		}
		built = |x| { count: 1, got: x }
		count : Count
		count = built(whole).count
		count.value
	}
}
