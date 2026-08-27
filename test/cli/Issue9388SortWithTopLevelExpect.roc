Issue9388SortWithTopLevelExpect :: [].{}

expect [3, 1, 2].sortWith(|a, b| if a < b LessThan else if a > b GreaterThan else Equal) == [1, 2, 3]
