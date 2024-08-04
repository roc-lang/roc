module { appId } -> [getUser, baseUrl, allUsers]

getUser : U32 -> Str
getUser = \userId ->
    "api.example.com/$(appId)/users/$(Num.toStr userId)"

allUsers : Str
allUsers =
    "$(baseUrl)/users"

baseUrl : Str
baseUrl =
    "api.example.com/$(appId)"
