module { appId, protocol } -> [baseUrl, getUser]

baseUrl : Str
baseUrl =
    protocol "api.example.com/$(appId)"


getUser : U32 -> Str
getUser = \userId ->
    protocol "api.example.com/$(appId)/users/$(Num.toStr userId)"
