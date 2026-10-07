import ByteRange
import Scalar

Shape := [].{
	ends : Str -> List(U64)
	ends = |source| {
		var $out = []
		for located in Scalar.iter(source) {
			$out = $out.append(ByteRange.end(located.byte_range) + Scalar.to_u32(located.scalar).to_u64())
		}
		$out
	}
}
