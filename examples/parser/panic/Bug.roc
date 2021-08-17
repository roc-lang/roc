interface Bug exposes [filterList, tests] imports [] 

isEven : I64 -> Bool
isEven = \n -> 
   when n % 2 is
      Ok 0 -> True
      Ok 1 -> False 
      Ok _ -> False
      Err _ -> False

filterList : List a, (a -> Bool) -> List a 
filterList = \list, predicate ->
   when List.first list is 
      Ok a -> if predicate a 
         then 
            filterList (List.drop list 1) predicate |> List.prepend a 
         else 
            filterList (List.drop list 1) predicate
      Err _ -> [ ] 


## TESTS

t1 = {name: "isEven", test: isEven 4}
t2 = {name: "filter even numbers", test: (filterList [1,2,3,4,5,6] isEven |> List.len) == 3}

tests = [t1, t2]
