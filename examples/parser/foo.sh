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
                