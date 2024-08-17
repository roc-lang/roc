module { appId, protocol } -> [baseUrl, getUser, getPost, getPostComments, getCompanies]

baseUrl : Str
baseUrl =
    protocol "api.example.com/$(appId)"


getUser : U32 -> Str
getUser = \userId ->
    # purposefully not using baseUrl to test top-level fn referencing param
    protocol "api.example.com/$(appId)/users/$(Num.toStr userId)"

getPost : U32 -> Str
getPost = \postId ->
    "$(baseUrl)/posts/$(Num.toStr postId)"


getPostComments : U32 -> Str
getPostComments = \postId ->
    "$(getPost postId)/comments"


getCompanies : List U32 -> List Str
getCompanies = \ids ->
    getCompany = \id ->
        protocol "api.example.com/$(appId)/companies/$(Num.toStr id)"

    List.map ids getCompany
