Issue9388SortWithTopLevelExpect :: [].{}

expect [3, 1, 2].sortWith(|a, b| if a < b FirstBeforeSecond else if a > b SecondBeforeFirst else Equivalent) == [1, 2, 3]
