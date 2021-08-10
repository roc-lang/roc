interface Loop exposes [  
      Step
      
       ] imports []



Step state a : [ Loop state, Done a ]

loop : (state -> Step state a), state -> a
loop nextState s =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done 