module { appId } -> [getUser, baseUrl]

getUser : U32 -> Str
getUser = \userId ->
    "api.example.com/$(appId)/users/$(Num.toStr userId)"


baseUrl : Str
baseUrl =
    "api.example.com/$(appId)"
