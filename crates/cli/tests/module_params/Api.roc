module { appId, protocol } -> [getUser]

getUser : U32 -> Str
getUser = \userId ->
    protocol "api.example.com/$(appId)/users/$(Num.toStr userId)"
