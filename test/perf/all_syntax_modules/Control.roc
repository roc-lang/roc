import Shared

Control := [].{
	question_postfix : List(Str) -> Try(I64, _)
	question_postfix = |strings| {
		first_str = strings.first()?
		first_num = I64.from_str(first_str)?

		Ok(first_num)
	}

	question_with_err_map : List(Str) -> Try(Str, _)
	question_with_err_map = |strings| {
		first_str = strings.first() ? NoFirstError
		Ok(first_str)
	}

	question_with_err_lambda : List(Str) -> Try(Str, _)
	question_with_err_lambda = |strings| {
		first_str = strings.first() ? |e| NoFirstError(e)
		Ok(first_str)
	}

	implement_me_later : Str -> Str
	implement_me_later = |str| {
		if str == "" {
			str
		} else {
			crash "not implemented"
		}
	}

	for_loop = |num_list| {
		var $sum = 0

		for num in num_list {
			$sum = $sum + num
		}

		$sum
	}

	break_in_for_loop = |bool_list| {
		var $all_true = True
		for b in bool_list {
			if b == False {
				$all_true = False
				break
			} else {
				{}
			}
		}
		$all_true
	}

	while_loop = |limit| {
		var $count = 0
		var $sum = 0

		while $count < limit {
			$sum = $sum + $count
			$count = $count + 1
		}

		$sum
	}

	if_demo : U64 -> Str
	if_demo = |num| {
		one_line_if = if (num == 1) "One" else "NotOne"

		two_line_if =
			if (num == 2)
				"Two"
			else
				"NotTwo"

		with_curlies =
			if Shared.is_small(num) {
				"Small"
			} else {
				"Large"
			}

		if (num == 3)
			"Three"
		else if (num == 4)
			"Four"
		else
			one_line_if.concat(two_line_if).concat(with_curlies)
	}
}
