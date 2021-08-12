interface Test exposes [  Test, passFail, eval, run, strListToStr ] imports [ ]

Test : { name : Str, test: Bool }

passFail : Bool -> Str 
passFail = \pass -> if pass then "Pass :: " else "Fail :: "

eval : Test -> Str
eval = \{ name, test } -> Str.concat (passFail test) name

run : List Test -> Str
run = \tests -> List.map tests eval |> strListToStr "\n"


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



