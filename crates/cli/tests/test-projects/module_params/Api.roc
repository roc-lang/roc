module { app_id, protocol } -> [
    base_url,
    get_user,
    get_post,
    get_posts,
    get_post_comments,
    get_companies,
    base_url_aliased,
    get_post_aliased,
    get_user_safe,
    get_post_comment,
]

## value def referencing params
base_url : Str
base_url =
    protocol("api.example.com/$(app_id)")

## function def referencing params
get_user : U32 -> Str
get_user = \user_id ->
    # purposefully not using baseUrl to test top-level fn referencing param
    protocol("api.example.com/$(app_id)/users/$(Num.to_str(user_id))")

## function def referencing top-level value
get_post : U32 -> Str
get_post = \post_id ->
    "$(base_url)/posts/$(Num.to_str(post_id))"

## function def passing top-level function
get_posts : List U32 -> List Str
get_posts = \ids ->
    List.map(ids, get_post)

## function def calling top-level function
get_post_comments : U32 -> Str
get_post_comments = \post_id ->
    "$(get_post(post_id))/comments"

## function def passing nested function
get_companies : List U32 -> List Str
get_companies = \ids ->
    get_company = \id ->
        protocol("api.example.com/$(app_id)/companies/$(Num.to_str(id))")

    List.map(ids, get_company)

## aliasing top-level value
base_url_aliased : Str
base_url_aliased =
    base_url

## aliasing top-level fn
get_post_aliased : U32 -> Str
get_post_aliased =
    get_post

## top-level value returning functions
get_user_safe : U32 -> Str
get_user_safe =
    if Str.starts_with(app_id, "prod_") then
        \id -> "$(get_user(id))?safe=true"
    else
        get_user

## two-argument function
get_post_comment : U32, U32 -> Str
get_post_comment = \post_id, comment_id ->
    "$(get_post(post_id))/comments/$(Num.to_str(comment_id))"
