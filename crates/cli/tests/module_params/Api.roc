module { appId } -> [
    baseUrl,
    allUsers,
    getUser,
    getUserPosts,
    getUserZero,
    postUrls,
]

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

getUserZero : Str
getUserZero =
    getUser 0

postUrls : U32 -> { details: Str, comments: Str, likes: Str }
postUrls = \postId ->
    subUrl = \sub ->
        "$(baseUrl)/posts/$(Num.toStr postId)/$(sub)"

    {
        details: subUrl "details",
        comments: subUrl "comments",
        likes: subUrl "likes",
    }
