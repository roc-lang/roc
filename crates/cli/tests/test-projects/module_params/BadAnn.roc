module { appId } -> [fnAnnotatedAsValue, missingArg]

fnAnnotatedAsValue : Str
fnAnnotatedAsValue = \postId, commentId ->
    "/posts/$(postId)/comments/$(Num.toStr commentId)"

missingArg : Str -> Str
missingArg = \postId, _ ->
    "/posts/$(postId)/comments"
