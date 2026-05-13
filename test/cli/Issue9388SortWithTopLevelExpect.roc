Issue9388SortWithTopLevelExpect :: [].{}

expect [3, 1, 2].sort_with(|a, b| if a < b LT else if a > b GT else EQ) == [1, 2, 3]
