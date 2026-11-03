ParallelDeferredErasedCalls :: [].{}

# Every step of a chain makes its erased call in tail position, which leaves
# the call pending for whoever waits on the procedure. Test workers run these
# chains at once, and each worker's pending calls must stay its own.

count_up : U64, (U64 -> U64) -> U64
count_up = |remaining, finish|
	if remaining == 0 {
		finish(0)
	} else {
		count_up(remaining - 1, |counted| finish(counted + 1))
	}

sum_up : U64, (U64 -> U64) -> U64
sum_up = |remaining, finish|
	if remaining == 0 {
		finish(0)
	} else {
		sum_up(remaining - 1, |total| finish(total + remaining))
	}

chain_length = 200000

expect count_up(chain_length, |counted| counted) == chain_length
expect count_up(chain_length + 1, |counted| counted) == chain_length + 1
expect count_up(chain_length + 2, |counted| counted) == chain_length + 2
expect count_up(chain_length + 3, |counted| counted) == chain_length + 3
expect count_up(chain_length + 4, |counted| counted) == chain_length + 4
expect count_up(chain_length + 5, |counted| counted) == chain_length + 5
expect count_up(chain_length + 6, |counted| counted) == chain_length + 6
expect count_up(chain_length + 7, |counted| counted) == chain_length + 7
expect sum_up(1000, |total| total) == 500500
expect sum_up(2000, |total| total) == 2001000
expect sum_up(3000, |total| total) == 4501500
expect sum_up(4000, |total| total) == 8002000
expect sum_up(5000, |total| total) == 12502500
expect sum_up(6000, |total| total) == 18003000
expect sum_up(7000, |total| total) == 24503500
expect sum_up(8000, |total| total) == 32004000
