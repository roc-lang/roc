module { appId } -> [baseUrl, allUsers, getUser, getUserPosts]

baseUrl : Str
baseUrl =
    "api.example.com/$(appId)"

allUsers : Str
allUsers =
    "$(baseUrl)/users"

getUser : U32 -> Str
getUser = \userId ->
    "$(allUsers)/$(Num.toStr userId)"

getUserPosts : U32 -> Str
getUserPosts = \userId ->
    "$(getUser userId)/posts"
