interface Loop exposes [  
      Step, loop, 
      Counter, updateCounter,
      test1, test2 ] imports []


# Reference: last part of https://jxxcarlson.medium.com/?p=63cd104a192a

Step state a : [ Loop state, Done a ]

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

test1 : Str 
test1 = 
     c = { counter : 0, value : 4}
     if updateCounter c == Done 4 then "Ok" else "Fail"

test2 : Str 
test2 = 
     c = { counter : 4, value : 0}
     if updateCounter c == Loop { counter : 3, value : 4} then "Ok" else "Fail"
       