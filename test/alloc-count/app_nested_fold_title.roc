app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for issue 11247. Both output lists have room for every scalar;
# appending through nested folds must preserve that unique storage.
Fact : { at : U64, upper : Bool }

Title : { out : List(U8), facts : List(Fact) }

title_word : Title, List(U8) -> Title
title_word = |state, word|
	List.fold_with_index(
		word,
		state,
		|acc, byte, index|
			{
				out: List.append(
					acc.out,
					if index == 0 {
						to_upper(byte)
					} else {
						to_lower(byte)
					},
				),
				facts: List.append(acc.facts, { at: List.len(acc.out), upper: index == 0 }),
			},
	)

to_title : List(List(U8)) -> Title
to_title = |words| {
	scalars = List.fold(words, 0, |total, word| total + List.len(word))
	initial = { out: List.with_capacity(scalars), facts: List.with_capacity(scalars) }
	List.fold(words, initial, title_word)
}

to_upper : U8 -> U8
to_upper = |byte| if byte >= 97 and byte <= 122 {
	byte - 32
} else {
	byte
}

to_lower : U8 -> U8
to_lower = |byte| if byte >= 65 and byte <= 90 {
	byte + 32
} else {
	byte
}

build_words : U64 -> List(List(U8))
build_words = |repetitions| {
	first = Str.to_utf8("ascii")
	second = Str.to_utf8("case")
	var $words = List.with_capacity(repetitions * 2)
	var $i = 0
	while $i < repetitions {
		$words = List.append(List.append($words, first), second)
		$i = $i + 1
	}
	$words
}

run! : Str => Str
run! = |input| {
	seed = Str.count_utf8_bytes(input)
	small_words = build_words(seed * 2)
	large_words = build_words(seed * 32)

	small_before = Host.alloc_count!()
	small_title = to_title(small_words)
	small_allocs = Host.alloc_count!() - small_before

	large_before = Host.alloc_count!()
	large_title = to_title(large_words)
	large_allocs = Host.alloc_count!() - large_before

	text_ok = Str.from_utf8_lossy(small_title.out) == Str.repeat("AsciiCase", seed * 2)
		and Str.from_utf8_lossy(large_title.out) == Str.repeat("AsciiCase", seed * 32)
	facts_ok = List.len(small_title.facts) == seed * 2 * 9
		and List.len(large_title.facts) == seed * 32 * 9
			and List.fold_with_index(small_title.facts, True, |ok, fact, index| ok and fact.at == index and fact.upper == (U64.rem_by(index, 9) == 0 or U64.rem_by(index, 9) == 5))
				and List.fold_with_index(large_title.facts, True, |ok, fact, index| ok and fact.at == index and fact.upper == (U64.rem_by(index, 9) == 0 or U64.rem_by(index, 9) == 5))

	# Optimized apps omit expects, so the runner checks this returned verdict.
	"title bytes: ${List.len(small_title.out).to_str()} ${List.len(large_title.out).to_str()}, text: ${
		if text_ok {
			"ok"
		} else {
			"wrong"
		}
	}, facts: ${
		if facts_ok {
			"ok"
		} else {
			"wrong"
		}
	}, allocations: ${small_allocs.to_str()} ${large_allocs.to_str()}"
}
