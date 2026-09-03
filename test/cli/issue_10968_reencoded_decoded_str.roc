# repro for https://github.com/roc-lang/roc/issues/10968
#
# `Json.parse` produces a record whose `element` field is `Try(record,
# [Missing])`. Matching that field binds the inner record, and the `guid` string
# it holds is re-encoded as a field of a fresh record. The re-encoded JSON has
# to carry that string's bytes, so this prints the encoded length along with
# whether the encoding is the JSON those bytes should produce.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

main! = |args| {
	# args[0] is the program path; args[1] carries the guid length, and every
	# byte of it stands for 16384 bytes of guid. Taking the length from the
	# command line keeps the JSON below out of reach of compile-time folding,
	# so the decode and the re-encode both happen at runtime on heap strings.
	#
	# The resulting guid is long enough to own its own allocation whose pages
	# go back to the OS when it is freed, so reading it after the free faults
	# rather than quietly handing back stale bytes.
	guid_len = Str.count_utf8_bytes(List.get(args, 1) ?? "") * 16384

	big = Str.from_utf8(List.repeat(97.U8, guid_len)).ok_or("x")
	json = "{\"element\":{\"guid\":\"${big}\"}}"

	decoded : Try({ element : Try({ guid : Str }, [Missing]) }, _)
	decoded = Json.parse(json)

	response =
		match decoded {
			Ok(m) => m
			Err(_) => crash "decode failed"
		}

	match response.element {
		Ok(element_ref) => {
			out = Json.to_str({ guid: element_ref.guid, method: "boundingBox" })

			# Built from a separate allocation, and only after the encode, so
			# comparing against it leaves the lifetimes the encode saw alone.
			guid_bytes = Str.repeat("a", guid_len)
			expected = "{\"guid\":\"${guid_bytes}\",\"method\":\"boundingBox\"}"

			verdict =
				if out == expected {
					"matches"
				} else {
					"differs"
				}

			Stdout.line!("bytes: ${Str.count_utf8_bytes(out).to_str()}, guid ${verdict}")
			Ok({})
		}
		Err(_) => Err(Exit(1))
	}
}
