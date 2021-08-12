interface Utility exposes [ strListToStr] imports [ ]


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
