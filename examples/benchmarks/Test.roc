interface Test exposes [  Test, passFail, evalTest, evalTests, eval, strListToStr ] imports [ ]

Test : { name : Str, test: Bool }

passFail : Bool -> Str 
passFail = \pass -> if pass then "Pass :: " else "Fail :: "

evalTest : Test -> Str
evalTest = \{ name, test } -> Str.concat (passFail test) name

evalTests : List Test -> List Str 
evalTests = \tests -> List.map tests evalTest

eval : List Test -> Str
eval = \tests -> evalTests tests |> strListToStr "; "

strListToStr : List Str, Str -> Str 
strListToStr = \list, separator -> 
    when List.first list is 
       Ok head -> 
            # TODO: Not great code following
            rest = List.drop list 1
              if List.len rest > 0 then 
                Str.concat (Str.concat head separator) (strListToStr rest separator)
              else 
                Str.concat head (strListToStr rest separator)
       Err _ -> "" 



