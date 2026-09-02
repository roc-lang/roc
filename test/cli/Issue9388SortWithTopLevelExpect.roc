Issue9388SortWithTopLevelExpect :: [].{}

expect [3, 1, 2].sort_with(|a, b| if a < b Before else if a > b After else Same) == [1, 2, 3]
