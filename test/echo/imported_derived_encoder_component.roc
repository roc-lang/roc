# Issue 10804 across a module boundary: `DerivedEncoderChart` derives
# `encoder_for` in its own module, and its field's type `DerivedEncoderData`
# declares none. Encoding a chart here is reported as `DerivedEncoderData`
# missing `encoder_for`, the type the program wrote, and running the program
# crashes at the encoder call.
import DerivedEncoderChart

main! = |_args| {
	echo!("before")
	echo!(Json.to_str(DerivedEncoderChart.make("foo.json")))
	echo!("after")
	Ok({})
}
