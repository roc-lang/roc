# Imported only by edited.roc, so its pack program is compiled by the edited
# app's warm build, which takes cache hits while specializing it: a wrapper
# it calls must not be one of them, or the pack would offer `total` calling
# it where every other program inlines it.
Extra := [].{
	total : List(U8) -> U64
	total = |list| {
		var $sum = list.len()
		var $index = 0
		while $index < list.len() {
			$sum = $sum + $index
			$index = $index + 1
		}
		$sum
	}
}
