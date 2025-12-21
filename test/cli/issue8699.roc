chunks = |arr, n| {
	var $res = []
	var $chunk = []

	for item in arr {
		if $chunk.len() == n {
			$res = $res.append($chunk)
			$chunk = [item]
		} else {
			$chunk = $chunk.append(item)
		}
	}

	if !$chunk.is_empty() {
		$res = $res.append($chunk)
	}

	$res
}

expect {
	# This expect is designed to fail to verify that test failures
	# are handled gracefully without panicking.
	# The actual result is [[1, 2, 3], [4]] but we expect something different.
	chunks([1, 2, 3, 4], 3) == [[1, 2]]
}
