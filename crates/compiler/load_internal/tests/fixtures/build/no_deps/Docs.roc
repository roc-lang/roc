## An interface for docs tests
interface Docs
    exposes [makeUser, getName]
    imports []

## This is a user
User : { name : Str }

## Makes a user
makeUser : Str -> User
makeUser = \name ->
    { name }

## gets the user's name
getName : User -> Str
getName = \a -> a.name

