module { app_id } -> [fn_annotated_as_value, missing_arg]

fn_annotated_as_value : Str
fn_annotated_as_value = \post_id, comment_id ->
    "/posts/$(post_id)/comments/$(Num.to_str(comment_id))"

missing_arg : Str -> Str
missing_arg = \post_id, _ ->
    "/posts/$(post_id)/comments"
