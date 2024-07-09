module { appId } -> [getUser]

getUser : U32 -> Str
getUser = \userId ->
    "api.example.com/$(appId)/users/$(Num.toStr userId)"
