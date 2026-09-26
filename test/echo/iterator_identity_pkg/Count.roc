import ByteRange
import Scalar

Count := [].{
	total : List(Str) -> U64
	total = |sources| {
		var $sum = 0
		for iter in sources.map(Scalar.iter) {
			for located in iter {
				$sum = $sum + ByteRange.end(located.byte_range)
			}
		}
		$sum
	}
}
