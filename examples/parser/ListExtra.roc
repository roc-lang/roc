interface ListExtra exposes [filter, tests] imports []


filter : List a, (a -> Bool) -> List a 
filter = \list, predicate ->
   when List.first list is 
      Ok a -> if predicate a 
         then 
            filter (List.drop list 1) predicate |> List.prepend a 
         else 
            filter (List.drop list 1) predicate
      Err _ -> [ ]  


## TESTS


isEven : I64 -> Bool
isEven = \n -> 
   when n % 2 is
      Ok 0 -> True
      Ok 1 -> False 
      _ -> False



t1 = {name: "filter [1,2,3,4,5,6] isEven == [2,4,6]", test: filter [1,2,3,4,5,6] isEven == [2,4,6]}

tests = [t1]     