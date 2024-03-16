## An interface for docs tests
interface Docs
    exposes [makeUser, getNameExposed]
    imports []

## This is a user
User : { name : Str }

## Makes a user
makeUser : Str -> User
makeUser = \name ->
    { name }

## Gets the user's name
getName = \a -> a.name

getNameExposed = getName

