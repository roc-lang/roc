module { appId, protocol } -> [baseUrl, getUser, getPost, getPostComments, getCompanies]

## value def referencing params
baseUrl : Str
baseUrl =
    protocol "api.example.com/$(appId)"

## function def referencing params
getUser : U32 -> Str
getUser = \userId ->
    # purposefully not using baseUrl to test top-level fn referencing param
    protocol "api.example.com/$(appId)/users/$(Num.toStr userId)"

## function def referencing top-level value that references param
getPost : U32 -> Str
getPost = \postId ->
    "$(baseUrl)/posts/$(Num.toStr postId)"

## function def referencing top-level function that references param
getPostComments : U32 -> Str
getPostComments = \postId ->
    "$(getPost postId)/comments"

## function def referencing nested function that references param
getCompanies : List U32 -> List Str
getCompanies = \ids ->
    getCompany = \id ->
        protocol "api.example.com/$(appId)/companies/$(Num.toStr id)"

    List.map ids getCompany
