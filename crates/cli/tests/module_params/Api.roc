module { appId, protocol } -> [
    baseUrl,
    getUser,
    getPost,
    getPosts,
    getPostComments,
    getCompanies,
    baseUrlAliased,
    getPostAliased,
    getUserSafe,
    getPostComment,
]

## value def referencing params
baseUrl : Str
baseUrl =
    protocol "api.example.com/$(appId)"

## function def referencing params
getUser : U32 -> Str
getUser = \userId ->
    # purposefully not using baseUrl to test top-level fn referencing param
    protocol "api.example.com/$(appId)/users/$(Num.toStr userId)"

## function def referencing top-level value
getPost : U32 -> Str
getPost = \postId ->
    "$(baseUrl)/posts/$(Num.toStr postId)"

## function def passing top-level function
getPosts : List U32 -> List Str
getPosts = \ids ->
    List.map ids getPost

## function def calling top-level function
getPostComments : U32 -> Str
getPostComments = \postId ->
    "$(getPost postId)/comments"

## function def passing nested function
getCompanies : List U32 -> List Str
getCompanies = \ids ->
    getCompany = \id ->
        protocol "api.example.com/$(appId)/companies/$(Num.toStr id)"

    List.map ids getCompany

## aliasing top-level value
baseUrlAliased : Str
baseUrlAliased =
    baseUrl

## aliasing top-level fn
getPostAliased : U32 -> Str
getPostAliased =
    getPost

## top-level value returning functions
getUserSafe : U32 -> Str
getUserSafe =
    if Str.startsWith appId "prod_" then
        \id -> "$(getUser id)?safe=true"
    else
        getUser

## two-argument function
getPostComment : U32, U32 -> Str
getPostComment = \postId, commentId ->
    "$(getPost postId)/comments/$(Num.toStr commentId)"
