# many : Parser a -> Parser (List a)
# many p =
#     Parser.loop [] (manyHelp p)


# manySeparatedBy : Parser () -> Parser a -> Parser (List a)
# manySeparatedBy sep p =
#     manyNonEmpty_ p (second sep p)


# manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
# manyHelp p vs =
#     Parser.oneOf
#         [ Parser.end EndOfInput |> Parser.map (\_ -> Parser.Done (List.reverse vs))
#         , Parser.succeed (\v -> Parser.Loop (v :: vs))
#             |= p
#         , Parser.succeed ()
#             |> Parser.map (\_ -> Parser.Done (List.reverse vs))
#         ]

manyAux = \p, as -> 
  if as == [] then 
    Loop.Done 
   
   oneOf [

   
   ]

many : Parser a -> Parser (List a)
many = \p -> 
          \input -> loop ()  [ ] 


many : Parser a -> Parser (List a)
many = \p ->
    loop (  \input -> ((\as -> (manyAux p as)) input)  ) [ ]          

manyAux : Parser a, List a -> Parser (Step (List a) (List a))
manyAux = \p, as ->
    \input -> if input == [] 
              then 
                succeed [] |> map (\_ -> Done (List.reverse as))
              else 
                oneOf [
                  andThen p (succeed (\a -> Loop (List.prepend as a))),
                  succeed [] |> map (\_ -> Done (List.reverse as))
                ]    

   q1 = {name: "test of isLowerCaseAlpha predicate with u = 97" test: isLowerCaseAlpha 97}
   q2 = {name: "test of isLowerCaseAlpha predicate with u = 122" test: isLowerCaseAlpha 122}
   q3 = {name: "test of isLowerCaseAlpha predicate with u = 96" test: isLowerCaseAlpha 96 == False}
   q4 = {name: "test of isLowerCaseAlpha predicate with u = 123" test: isLowerCaseAlpha 123 == False}



# manyAuxFAKE : Parser a, List a -> Parser (Step (List a) (List a))
# manyAuxFAKE = \_, _ ->
#     \input -> (succeed (Done [])) input
              
# manyAuxFAKE2 : Parser a, List a -> Parser (Step (List a) (List a))
# manyAuxFAKE2 = \p, list ->
#     \input -> (andThen p (\a -> (succeed (Done list)))) input


loop : (state -> Parser (Step state a)), state -> Parser a 
loop = \nextState, s ->
  when nextState s is 
     Loop ss -> loop nextState ss
     Done aa -> aa

loop : (state -> Parser (Step state a)), state -> Parser a 
loop = \nextState, s ->
  \input -> 
      ss =  (nextState s)
      when ss is 
        Loop ss -> 
          out = ss input
          if List.len out == 1 then
            Loop nextState ss
          else Done (\input -> out)
        Done aa -> (\input -> success aa)

loop : (state -> Parser (Step state a)), state -> Parser a 
loop = \nextState, s ->
  \input -> 
      ps =  (nextState s)                 # Parser (Step state a))
      when ps is 
          Parser (Loop s2) ->
            output = ps input             # List [Pair (Step state a) (List U8)]
            when List.head output is      # [Pair (Step state a) (List U8)]
              Ok (Pair aa input2) -> 
                if List.len output == 1
                then 
                    (loop nextState ps ) input2
                else 
                    []

          Parser (Done aa) -> [Pair aa input]
                
app "app"
     packages { base: "platform" }
     imports [base.Task, Test,ListExtra] 
     provides [ main ] to base

main : Task.Task {} []
main =
  Test.run ListExtra.tests "List Extra"
    |> Task.putLine 



filter : List a, (a -> Bool) -> List a 
filter = \list, predicate ->
   when List.first list is 
      Ok a -> if predicate a 
         then 
            filter (List.drop list 1) predicate |> List.prepend a 
         else 
            filter (List.drop list 1) predicate
      Err _ -> [ ]  

isEven : I64 -> Bool
isEven = \n -> 
   when n % 2 is
      Ok 0 -> True
      Ok 1 -> False 
      _ -> False

t1 = {name: "filter [1,2,3,4,5,6] isEven == [2,4,6]", test: filter [1,2,3,4,5,6] isEven == [2,4,6]}

tests = [t1]     

oop : (state -> Parser (Step state a)), state -> Parser a 
loop = \nextState, s ->
  \_input -> 
      ps =  (nextState s)                 # Parser (Step state a))
      when ps input s 
          List (Pair (Loop ss) input2) -> []
          List (Pair (Done aa) input2) -> []     


loop : (state -> Parser (Step (List a) (List a))), state -> Parser (List a) 
loop = \nextState, s ->
  \input -> 
      ps =  (nextState s)                 # Parser (Step (List a)(List a))
      out = ps input                      # List (Pair (Step (List a)(List a)) (List U8))
      if List.len out != 1 then [Pair (Done []) input]
      else 
          when List.first out is 
            Ok (Pair (Loop aas) input2) -> [Pair (Loop aas) input2]
            Ok (Pair (Done aas) input2) -> [Pair (Done aas) input2]
            Err _ -> [Pair (Done []) input]

# state : List a
#
loop : (state -> Parser (Step (List a) (List a))), state -> Parser (List a) 
loop = \nextState, s ->
  \input -> 
      ps =  (nextState s)                 # Parser (Step (List a)(List a))
      out = ps input                      # List (Pair (Step (List a)(List a)) (List U8))
      if List.len out != 1 then [Pair [] input]
      else 
          when List.first out is 
            Ok (Pair (Loop aas) input2) -> (nextState aas) input2 
            Ok (Pair (Done aas) input2) -> [Pair aas input2]
            Err _ -> [Pair [] input]            

loop : (state -> Parser (Step (List a) (List a))), state -> Parser (List a) 
loop = \nextState, s ->
  \input -> 
      ps =  (nextState s)                 # Parser (Step (List a)(List a))
      out = ps input                      # List (Pair (Step (List a)(List a)) (List U8))
      if List.len out != 1 then [Pair [] input]
      else 
          when List.first out is 
            Ok (Pair (Loop aas) input2) -> (nextState aas) input2 
            Ok (Pair (Done aas) input2) -> [Pair aas input2]
            Err _ -> [Pair [] input] 