interface Url
    exposes [Url, fromStr]
    imports []

Url := Str

## Create a [Url] without validating or percent-encoding anything.
fromStr : Str -> Url
fromStr = \str -> @Url str

toStr : Url -> Str
toStr = \@Url str -> str

## [Percent-encodes](https://en.wikipedia.org/wiki/Percent-encoding) a
## [path component](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Syntax)
## and appends to the end of the URL's path.
##
##     "https://example.com"
##         |> Url.append "some stuff"
##     # https://example.com/some%20stuff
##
## This will be appended before any queries and fragments.
##
##     "https://example.com?search=blah#fragment"
##         |> Url.append "stuff"
##     # https://example.com/stuff?search=blah#fragment
##
## If the given string begins with a "/" and the URL already ends with a "/", one of
## them will be ignored. This avoids turning a single slash into a double slash.
##
##     "https://example.com/things/"
##         |> Url.append "/stuff/"
##         |> Url.append "/more/etc/"
##     # https://example.com/things/stuff/more/etc/"
##
## If either the given URL or the given string is empty, no "/" will be added.
##
##     "https://example.com/things"
##         |> Url.append ""
##     # https://example.com/things
append : Url, Str -> Url
append = \@Url urlStr, suffixUnencoded ->
    suffix = percentEncode suffixUnencoded

    when Str.splitFirst urlStr "?" is
        Ok { before, after } ->
            # TODO use Str.reserve once it exists
            appendHelp before suffix
                |> Str.append "?"
                |> Str.append after
                |> @Url
        Err NotFound ->
            # There wasn't a query, but there might still be a fragment
            when Str.splitFirst urlStr "#" is
                Ok { before, after } ->
                    # TODO use Str.reserve once it exists
                    appendHelp before suffix
                        |> Str.append "#"
                        |> Str.append after
                        |> @Url
                Err NotFound ->
                    # No query and no fragment, so just append it
                    @Url (appendHelp urlStr suffix)

## Internal helper
appendHelp : Str, Str -> Str
appendHelp = \prefix, suffix ->
    if Str.endsWith prefix "/" then
        if Str.startsWith suffix "/" then
            # Avoid a double-slash by appending only the part of the suffix after the "/"
            when Str.splitFirst suffix "/" is
                Ok { after } ->
                    # TODO `expect before == ""`
                    Str.append prefix after
                Err NotFound ->
                    # This should never happen, because we already verified
                    # that the suffix startsWith "/"
                    # TODO `expect False` here with a comment
                    Str.append prefix suffix
        else
            # prefix ends with "/" but suffix doesn't start with one, so just append.
            Str.append prefix suffix
    else
        if Str.startsWith suffix "/" then
            # Suffix starts with "/" but prefix doesn't end with one, so just append them.
            Str.append prefix suffix
        else if Str.isEmpty prefix then
            # Prefix is empty; return suffix.
            suffix
        else if Str.isEmpty suffix then
            # Suffix is empty; return prefix.
            prefix
        else
            # Neither is empty, but neither has a "/", so add one in between.
            # TODO use Str.reserve once it exists
            prefix
                |> Str.append "/"
                |> Str.append suffix

## Internal helper. This is intentionally unexposed so that you don't accidentally
## double-encode things. If you want to percent-encode something, you can always do:
##
##     Path.fromStr ""
##         |> Path.append myStrToEncode
##         |> Path.toStr
##
## Note that it's not necessary to situationally encode spaces as `+` instead of `%20` -
## it's apparently always safe to use `%20` (but not always safe to use `+`):
## https://stackoverflow.com/questions/2678551/when-should-space-be-encoded-to-plus-or-20/47188851#47188851
percentEncode : Str -> Str
percentEncode = \input ->
    # TODO `Str.replaceUtf8 : Str, (U8 -> [Same, Replace Str]) -> Str` can let you translate UTF-8 bytes into strings.
    #
    # TODO instead of starting with "", start with Str.withCapacity based on
    # some the length of the given str. (Maybe be optimistic and assume it's the same length,
    # then let it get doubled if we're wrong.)
    #
    # https://www.ietf.org/rfc/rfc3986.txt
    List.replaceUtf8 input \byte ->
        if
            (byte >= 97 && byte <= 122) # lowercase ASCII
            || (byte >= 65 && byte <= 90) # uppercase ASCII
            || (byte >= 48 && byte <= 57) # digit
        then
            # This is the most common case: an unreserved character,
            # which needs no encoding in a path
            Same
        else
            when byte is
                46 # '.'
                | 95 # '_'
                | 126 # '~'
                | 150 -> # '-'
                    # These special characters can all be unescaped in paths
                    Same

                _ ->
                    # This needs encoding in a path
                    hex = Num.toHexUppercase byte

                    Replace "%\(hex)"

## Adds a [Str] query parameter to the end of the [Url]. Both the key
## and the value are [percent-encoded](https://en.wikipedia.org/wiki/Percent-encoding).
##
##     "https://example.com"
##         |> Url.param "email" "someone@example.com"
##     # https://example.com?email=someone%40example.com
##
## This can be called multiple times on the same URL.
##
##     "https://example.com"
##         |> Url.param "café" "du Soleil"
##         |> Url.param "email" "someone@example.com"
##     # https://example.com?caf%C3%A9=du%20Soleil&email=someone%40example.com
param : Url, Str, Str -> Url
param = \@Url urlStr, key, value ->
    # TODO use Str.reserve once it exists
    urlStr
        |> Str.append (if hasQuery (@Url urlStr) then "&" else "?")
        |> Str.append key
        |> Str.append "="
        |> Str.append value
        |> @Url

## Returns `True` if the URL has a `?` in it.
hasQuery : Url -> Bool
hasQuery = \@Url urlStr ->
    # TODO use Str.contains once it exists. It should have a "fast path"
    # with SIMD iteration if the string is small enough to fit in a SIMD register.
    Str.toUtf8 urlStr
        |> List.contains '?'
