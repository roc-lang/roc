## An interface for docs tests
module [make_user, get_name_exposed]

## This is a user
User : { name : Str }

## Makes a user
##
## Takes a name Str.
make_user : Str -> User
make_user = \name ->
    { name }

## Gets the user's name
get_name = \a -> a.name

get_name_exposed = get_name

