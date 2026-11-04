# Under `--specialize=no` each module lowers on its own, and the pieces of all of
# them meet in one program. `Letters` and `Numbers` have the same shape, so each
# one's generated workers land at the same positions in its own lowering, where
# a conversion or a derived encoder of one module does not work for the other.
#
# Every value here comes from the program's arguments, so the workers run in the
# built program and not at compile time.
import Letters
import Numbers

line! = |text| echo!("${text}\n")

main! = |args| {
	none = args.drop_first(args.len())
	text = "a string long enough to be on the heap"
	letters = none.append(text)
	numbers = List.map(letters, |letter| letter.count_utf8_bytes().to_u16_wrap())
	number = List.fold(numbers, 0, |total, n| total + n)

	line!(Str.inspect(Letters.widened(letters)))
	line!(Str.inspect(Numbers.widened(numbers)))
	line!(Str.inspect(Letters.widened(none)))
	line!(Str.inspect(Numbers.widened(List.map(none, |letter| letter.count_utf8_bytes().to_u16_wrap()))))
	line!(Str.inspect(Letters.widened(letters.concat(letters).concat(letters))))
	line!(Str.inspect(Numbers.widened(numbers.concat(numbers).concat(numbers))))
	line!(Str.inspect((Letters.same(text, text), Letters.same(text, "other"))))
	line!(Str.inspect((Numbers.same(number, number), Numbers.same(number, 1))))
	line!(Letters.encoded(text))
	line!(Numbers.encoded(number))
	Ok({})
}
