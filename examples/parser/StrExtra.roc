interface StrExtra exposes [concat, join, repeat, tests] imports [ ]

repeat: Nat, Str -> Str
repeat = \n, str -> List.repeat n str |> concat

concat : List Str -> Str 
concat = \list -> 
    when List.first list is 
       Ok head -> 
           Str.concat head (concat (List.drop list 1))
       Err _ -> "" 


join : List Str, Str -> Str 
join = \list, separator -> 
    when List.first list is 
       Ok head -> 
            # TODO: Not great code following
            rest = List.drop list 1
              if List.len rest > 0 then 
                Str.concat (Str.concat head separator) (join rest separator)
              else 
                Str.concat head (join rest separator)
       Err _ -> "" 


t1 = { name: "repeat", test: repeat 4 "A" == "AAAA" }
t2 = { name: "concat", test: concat ["A", "B", "C"] == "ABC" }
t3 = { name: "join", test: join ["A", "B", "C"] "." == "A.B.C" }

tests = [t1,t2,t3]