interface Loop exposes [  
      Step, loop,
      Counter, updateCounter,
      tests ] imports []


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
           

Counter : { counter : I64, value : I64 }

updateCounter : Counter -> Step Counter I64
updateCounter = \state -> 
        when state.counter is
            0 ->
                Done state.value
            _ ->
                Loop { state & counter : state.counter - 1, value : state.value + state.counter }



t1 = {name: "updateCounter { counter : 0, value : 4} => Done 4", test : updateCounter { counter : 0, value : 4} == Done 4}
t2 = {name: "updateCounter { counter : 4, value : 0} => Loop { counter : 3, value : 4} ", test : updateCounter { counter : 4, value : 0} == Loop { counter : 3, value : 4}}
t3 = {name: "loop updateCounter { counter : 5, value : 0} =>  15", test : loop updateCounter { counter : 5, value : 0} == 15}
t4 = {name: "doneBranch (updateCounter { counter : 0, value : 4}) => Ok 4", test : doneBranch (updateCounter { counter : 0, value : 4}) == Ok 4}
t5 = {name: "doneBranch (updateCounter { counter : 4, value : 0}) => Err \"You applied this to a Loop value\"", test : doneBranch (updateCounter { counter : 4, value : 0}) == Err "You applied this to a Loop value" }
t6 = {name: "loopBranch (updateCounter { counter : 4, value : 0}) => Ok { counter : 3, value : 4}", test : loopBranch (updateCounter { counter : 4, value :0}) == Ok { counter : 3, value : 4} }
t7 = {name: "loopBranch (updateCounter { counter : 0, value : 4}) => Err \"You applied this to a Done value\"", test : loopBranch (updateCounter { counter : 0, value :4}) == Err "You applied this to a Done value" }

tests = [t1,t2,t3, t4, t5, t6, t7]



       