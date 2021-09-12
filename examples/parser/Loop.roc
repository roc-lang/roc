interface Loop exposes [  
      Step, loop, loopBranch, doneBranch ] imports []


# Reference: last part of https://jxxcarlson.medium.com/?p=63cd104a192a

Step state a : [ Loop state, Done a ]

doneBranch : Step state a -> Result a Str
doneBranch  =  \step -> 
   when step is 
      Done a -> Ok a 
      Loop _ -> Err "You applied this to a Loop value"


loopBranch : Step state a -> Result state Str
loopBranch  = \step -> 
  when step is 
      Loop x -> Ok x 
      Done _ -> Err "You applied this to a Done value"      


loop : (state -> Step state a), state -> a
loop = \nextState, s ->
    when nextState s is
        Loop ss ->
            loop nextState ss
        Done aa -> aa
           



       