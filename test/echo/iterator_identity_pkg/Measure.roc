import ByteRange
import Scalar

Measure := [].{
	starts : Str -> List(U64)
	starts = |source| {
		var $out = []
		for located in Scalar.iter(source) {
			$out = $out.append(ByteRange.start(located.byte_range) + located.scalar_index)
		}
		$out
	}
}
