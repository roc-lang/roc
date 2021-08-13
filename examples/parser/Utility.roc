interface Utility exposes [ concatStrList, concatStrListWithSeparator, 
   isEven, filterList,
   spaceAboveBelow,
   showU8Pair, showU8,
   tests] imports [ ]


## Lists  


filterList : List a, (a -> Bool) -> List a 
filterList = \list, predicate ->
   when List.first list is 
      Ok a -> if predicate a 
         then 
            filterList (List.drop list 1) predicate |> List.prepend a 
         else 
            filterList (List.drop list 1) predicate
      Err _ -> [ ]      


## Strings


spaceAboveBelow: Str -> Str
spaceAboveBelow = \str -> concatStrList ["\n", str, "\n"]


concatStrList : List Str -> Str 
concatStrList = \list -> 
    when List.first list is 
       Ok head -> 
           Str.concat head (concatStrList (List.drop list 1))
       Err _ -> "" 



concatStrListWithSeparator : List Str, Str -> Str 
concatStrListWithSeparator = \list, separator -> 
    when List.first list is 
       Ok head -> 
            # TODO: Not great code following
            rest = List.drop list 1
              if List.len rest > 0 then 
                Str.concat (Str.concat head separator) (concatStrListWithSeparator rest separator)
              else 
                Str.concat head (concatStrListWithSeparator rest separator)
       Err _ -> "" 



## Pairs / U8

showU8 : U8 -> Str
showU8 = 
   \u -> when Str.fromUtf8 [u] is 
      Ok str -> str
      _ -> "Oops, could not convert U8"


showU8Pair : [Pair U8 (List U8)] -> Str
showU8Pair = \(Pair a b) -> 
    when Str.fromUtf8 b is 
        Ok bb -> 
            concatStrList ["(", showU8 a, ", ", bb, ")"]
        _ -> "Could not convert (List U8)"



## TESTS

isEven : I64 -> Bool
isEven = \n -> 
   when n % 2 is
      Ok 0 -> True
      Ok 1 -> False 
      _ -> False
   

t1 = {name: "concatStrList [\"a\", \"b\", \"c\"] == \"abc\"", test: concatStrList ["a", "b", "c"] == "abc"}
t2 = {name: "concatStrListWithSeparator [\"a\", \"b\", \"c\"] \".\" == \"a.b.c\"", test: concatStrListWithSeparator ["a", "b", "c"] "." == "a.b.c" }
t3 = {name: "isEven 4 == True", test: isEven 4}
t4 = {name: "isEven 5 == False", test: isEven 5 == False}
t5 = {name: "filterList [1,2,3,4,5,6] isEven == [2,4,6]", test: filterList [1,2,3,4,5,6] isEven == [2,4,6]}
t6 = {name: "showU8 97 == \"a\"", test: showU8 97 == "a" }
t7 = {name: "showU8Pair (Pair 97 [98, 99]) == \"(a, bc)\"", test: showU8Pair (Pair 97 [98, 99]) == "(a, bc)"}

tests = [t1, t2, t3, t4, t5, t6, t7]