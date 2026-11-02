expect [3.I64, 1, 2].iter().first() == Ok(3)

expect [1.I64, 2, 3, 4].iter().keep_if(|number| number > 2).first() == Ok(3)

expect [7.I64].iter().drop_first(1).first() == Err(IterWasEmpty)

expect List.iter([]).first() == Err(IterWasEmpty)

expect [1.I64, 2].iter().keep_if(|number| number > 9).first() == Err(IterWasEmpty)

expect [8.I64].iter().first() == Ok(8)

expect [1.I64, 2, 3].iter().sum() == 6

expect [2.I64, 3, 4].iter().product() == Ok(24)

expect [3.I64, 1, 2].iter().min() == Ok(1)

expect [3.I64, 1, 2].iter().max() == Ok(3)
