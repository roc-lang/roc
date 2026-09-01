AffineCipher :: { a : U64, b : U64, encode_map : List(U8), decode_map : List(U8) }.{
	alphabet_size : U64
	alphabet_size = 26

	group_length : U64
	group_length = 5

	new : { a : U64, b : U64 } -> Try(AffineCipher, [InvalidKey])
	new = |{ a, b }| {
		encode_map : List(U8)
		encode_map =
			(0..<alphabet_size)
				.iter()
				.map(
					|index| {
						encoded_index = (a * index + b) % alphabet_size
						'a' + (
							encoded_index.to_u8_try() ?? {
								crash "Unreachable"
							}
						)
					},
				)
				|> List.from_iter

		if Set.from_list(encode_map).len() < encode_map.len() {
			Err(InvalidKey)
		} else {
			decode_map : List(U8)
			decode_map =
				encode_map
					.map_with_index(
						|encoded, decoded_index| { encoded, decoded_index },
					)
					.sort_with(
						|{ encoded: encoded1, decoded_index: _ }, { encoded: encoded2, decoded_index: _ }| {
							if encoded1 < encoded2 {
								Before
							} else if encoded1 > encoded2 {
								After
							} else {
								Same
							}
						},
					)
					.map(
						|pair| {
							(
								pair.decoded_index.to_u8_try() ?? {
									crash "Unreachable"
								}
							) + 'a'
						},
					)

			Ok({ a, b, encode_map, decode_map })
		}
	}

	encode : AffineCipher, Str -> Str
	encode = |affine_cipher, phrase| {
		maybe_result = (
			phrase
				.to_utf8()
				.join_map(
					|char| {
						if char >= '0' and char <= '9' {
							[char]
						} else {
							char_lower = if char >= 'A' and char <= 'Z' {
								char - 'A' + 'a'
							} else {
								char
							}
							if char_lower >= 'a' and char_lower <= 'z' {
								index = U8.to_u64(char_lower) - 'a'
								match affine_cipher.encode_map.get(index) {
									Ok(encoded_char) => [encoded_char]
									Err(OutOfBounds) => {
										crash "Unreachable: index cannot be out of bounds here"
									}
								}
							} else {
								[]
							}
						}
					},
				)
				.chunks_of(group_length)
				.intersperse([' '])
		).join()
			|> Str.from_utf8
		match maybe_result {
			Ok(result) => result
			Err(_) => {
				crash "Unreachable: ASCII characters are always valid UTF-8"
			}
		}
	}

	decode : AffineCipher, Str -> Try(Str, [BadUtf8(_), InvalidCharacter])
	decode = |affine_cipher, phrase| {
		phrase
			.to_utf8()
			.map_try(
				|char| {
					if char == ' ' {
						Ok([])
					} else if char >= '0' and char <= '9' {
						Ok([char])
					} else if char >= 'a' and char <= 'z' {
						index = U8.to_u64(char) - 'a'
						match affine_cipher.decode_map.get(index) {
							Ok(decoded_char) => Ok([decoded_char])
							Err(OutOfBounds) => {
								crash "Unreachable: index cannot be out of bounds here"
							}
						}
					} else {
						Err(InvalidCharacter)
					}
				},
			)?
			.join()
			|> Str.from_utf8
	}
}

##
## encode
##

# encode yes
expect {
	phrase = "yes"
	affine_cipher = AffineCipher.new({ a: 5, b: 7 })?
	result = affine_cipher.encode(phrase)
	expected = "xbt"
	result == expected
}

# encode no
expect {
	phrase = "no"
	affine_cipher = AffineCipher.new({ a: 15, b: 18 })?
	result = affine_cipher.encode(phrase)
	expected = "fu"
	result == expected
}

# encode OMG
expect {
	phrase = "OMG"
	affine_cipher = AffineCipher.new({ a: 21, b: 3 })?
	result = affine_cipher.encode(phrase)
	expected = "lvz"
	result == expected
}

# encode O M G
expect {
	phrase = "O M G"
	affine_cipher = AffineCipher.new({ a: 25, b: 47 })?
	result = affine_cipher.encode(phrase)
	expected = "hjp"
	result == expected
}

# encode mindblowingly
expect {
	phrase = "mindblowingly"
	affine_cipher = AffineCipher.new({ a: 11, b: 15 })?
	result = affine_cipher.encode(phrase)
	expected = "rzcwa gnxzc dgt"
	result == expected
}

# encode numbers
expect {
	phrase = "Testing,1 2 3, testing."
	affine_cipher = AffineCipher.new({ a: 3, b: 4 })?
	result = affine_cipher.encode(phrase)
	expected = "jqgjc rw123 jqgjc rw"
	result == expected
}

# encode deep thought
expect {
	phrase = "Truth is fiction."
	affine_cipher = AffineCipher.new({ a: 5, b: 17 })?
	result = affine_cipher.encode(phrase)
	expected = "iynia fdqfb ifje"
	result == expected
}

# encode all the letters
expect {
	phrase = "The quick brown fox jumps over the lazy dog."
	affine_cipher = AffineCipher.new({ a: 17, b: 33 })?
	result = affine_cipher.encode(phrase)
	expected = "swxtj npvyk lruol iejdc blaxk swxmh qzglf"
	result == expected
}

# encode with a not coprime to m
expect {
	affine_cipher = AffineCipher.new({ a: 6, b: 17 })
	affine_cipher.is_err()
	# AffineCipher could not be created, so cannot encode or decode
}

##
## decode
##

# decode exercism
expect {
	phrase = "tytgn fjr"
	affine_cipher = AffineCipher.new({ a: 3, b: 7 })?
	result = affine_cipher.decode(phrase)
	expected = Ok("exercism")
	result == expected
}

# decode a sentence
expect {
	phrase = "qdwju nqcro muwhn odqun oppmd aunwd o"
	affine_cipher = AffineCipher.new({ a: 19, b: 16 })?
	result = affine_cipher.decode(phrase)
	expected = Ok("anobstacleisoftenasteppingstone")
	result == expected
}

# decode numbers
expect {
	phrase = "odpoz ub123 odpoz ub"
	affine_cipher = AffineCipher.new({ a: 25, b: 7 })?
	result = affine_cipher.decode(phrase)
	expected = Ok("testing123testing")
	result == expected
}

# decode all the letters
expect {
	phrase = "swxtj npvyk lruol iejdc blaxk swxmh qzglf"
	affine_cipher = AffineCipher.new({ a: 17, b: 33 })?
	result = affine_cipher.decode(phrase)
	expected = Ok("thequickbrownfoxjumpsoverthelazydog")
	result == expected
}

# decode with no spaces in input
expect {
	phrase = "swxtjnpvyklruoliejdcblaxkswxmhqzglf"
	affine_cipher = AffineCipher.new({ a: 17, b: 33 })?
	result = affine_cipher.decode(phrase)
	expected = Ok("thequickbrownfoxjumpsoverthelazydog")
	result == expected
}

# decode with too many spaces
expect {
	phrase = "vszzm    cly   yd cg    qdp"
	affine_cipher = AffineCipher.new({ a: 15, b: 16 })?
	result = affine_cipher.decode(phrase)
	expected = Ok("jollygreengiant")
	result == expected
}

# decode with a not coprime to m
expect {
	affine_cipher = AffineCipher.new({ a: 13, b: 5 })
	affine_cipher.is_err()
	# AffineCipher could not be created, so cannot encode or decode
}
