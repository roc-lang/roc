interface Url
    exposes [
        Url,
        append,
        fromStr,
        toStr,
        appendParam,
        hasQuery,
        hasFragment,
        query,
        fragment,
        reserve,
        withQuery,
        withFragment,
    ]
    imports []

## A [Uniform Resource Locator](https://en.wikipedia.org/wiki/URL).
##
## It could be an absolute address, such as `https://roc-lang.org/authors` or
## a relative address, such as `/authors`. You can create one using [Url.fromStr].
Url := Str

## Reserve the given number of bytes as extra capacity. This can avoid reallocation
## when calling multiple functions that increase the length of the URL.
##
##     Url.fromStr "https://example.com"
##         |> Url.reserve 50 # We're about to add 50 UTF-8 bytes to it
##         |> Url.append "stuff"
##         |> Url.appendParam "café" "du Monde"
##         |> Url.appendParam "email" "hi@example.com"
##     # https://example.com/stuff?caf%C3%A9=du%20Monde&email=hi%40example.com
##
## The [Str.countUtf8Bytes] function can be helpful in finding out how many bytes to reserve.
##
## There is no `Url.withCapacity` because it's better to reserve extra capacity
## on a [Str] first, and then pass that string to [Url.fromStr]. This function will make use
## of the extra capacity.
reserve : Url, Nat -> Url
reserve = \@Url str, cap ->
    @Url (Str.reserve str cap)

## Create a [Url] without validating or [percent-encoding](https://en.wikipedia.org/wiki/Percent-encoding)
## anything.
##
##     Url.fromStr "https://example.com#stuff"
##     # https://example.com#stuff
##
## URLs can be absolute, like `https://example.com`, or they can be relative, like `/blah`.
##
##     Url.fromStr "/this/is#relative"
##     # /this/is#relative
##
## Since nothing is validated, this can return invalid URLs.
##
##     Url.fromStr "https://this is not a valid URL, not at all!"
##     # https://this is not a valid URL, not at all!
##
## Naturally, passing invalid URLs to functions that need valid ones will tend to result in errors.
fromStr : Str -> Url
fromStr = \str -> @Url str

## Return a [Str] representation of this URL.
##
##     Url.fromStr "https://example.com"
##         |> Url.append "two words"
##         |> Url.toStr
##     # "https://example.com/two%20words"
toStr : Url -> Str
toStr = \@Url str -> str

## [Percent-encodes](https://en.wikipedia.org/wiki/Percent-encoding) a
## [path component](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Syntax)
## and appends to the end of the URL's path.
##
##     Url.fromStr "https://example.com"
##         |> Url.append "some stuff"
##     # https://example.com/some%20stuff
##
## This will be appended before any queries and fragments.
##
##     Url.fromStr "https://example.com?search=blah#fragment"
##         |> Url.append "stuff"
##     # https://example.com/stuff?search=blah#fragment
##
## If the given path string begins with `"/"` and the URL already ends with `"/"`, one
## will be ignored. This avoids turning a single slash into a double slash.
##
##     Url.fromStr "https://example.com/things/"
##         |> Url.append "/stuff/"
##         |> Url.append "/more/etc/"
##     # https://example.com/things/stuff/more/etc/"
##
## If either the given URL or the given string is empty, no `"/"` will be added.
##
##     Url.fromStr "https://example.com/things"
##         |> Url.append ""
##     # https://example.com/things
append : Url, Str -> Url
append = \@Url urlStr, suffixUnencoded ->
    suffix = percentEncode suffixUnencoded

    when Str.splitFirst urlStr "?" is
        Ok { before, after } ->
            bytes =
                Str.countUtf8Bytes before
                + 1 # for "/"
                + Str.countUtf8Bytes suffix
                + 1 # for "?"
                + Str.countUtf8Bytes after

            before
            |> Str.reserve bytes
            |> appendHelp suffix
            |> Str.concat "?"
            |> Str.concat after
            |> @Url

        Err NotFound ->
            # There wasn't a query, but there might still be a fragment
            when Str.splitFirst urlStr "#" is
                Ok { before, after } ->
                    bytes =
                        Str.countUtf8Bytes before
                        + 1 # for "/"
                        + Str.countUtf8Bytes suffix
                        + 1 # for "#"
                        + Str.countUtf8Bytes after

                    before
                    |> Str.reserve bytes
                    |> appendHelp suffix
                    |> Str.concat "#"
                    |> Str.concat after
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
                    Str.concat prefix after

                Err NotFound ->
                    # This should never happen, because we already verified
                    # that the suffix startsWith "/"
                    # TODO `expect Bool.false` here with a comment
                    Str.concat prefix suffix
        else
            # prefix ends with "/" but suffix doesn't start with one, so just append.
            Str.concat prefix suffix
    else if Str.startsWith suffix "/" then
        # Suffix starts with "/" but prefix doesn't end with one, so just append them.
        Str.concat prefix suffix
    else if Str.isEmpty prefix then
        # Prefix is empty; return suffix.
        suffix
    else if Str.isEmpty suffix then
        # Suffix is empty; return prefix.
        prefix
    else
        # Neither is empty, but neither has a "/", so add one in between.
        prefix
        |> Str.concat "/"
        |> Str.concat suffix

## Internal helper. This is intentionally unexposed so that you don't accidentally
## double-encode things. If you really want to percent-encode an arbitrary string,
## you can always do:
##
##     Url.fromStr ""
##         |> Url.append myStrToEncode
##         |> Url.toStr
##
## Note that it's not necessary to situationally encode spaces as `+` instead of `%20` -
## it's apparently always safe to use `%20` (but not always safe to use `+`):
## https://stackoverflow.com/questions/2678551/when-should-space-be-encoded-to-plus-or-20/47188851#47188851
percentEncode : Str -> Str
percentEncode = \input ->
    # Optimistically assume we won't need any percent encoding, and can have
    # the same capacity as the input string. If we're wrong, it will get doubled.
    initialOutput = strWithCapacity (Str.countUtf8Bytes input)

    # TODO use Str.walkUtf8 once it exists
    Str.walkUtf8WithIndex input initialOutput \output, byte, _index ->
        # Spec for percent-encoding: https://www.ietf.org/rfc/rfc3986.txt
        if
            (byte >= 97 && byte <= 122) # lowercase ASCII
            || (byte >= 65 && byte <= 90) # uppercase ASCII
            || (byte >= 48 && byte <= 57) # digit
        then
            # This is the most common case: an unreserved character,
            # which needs no encoding in a path
            Str.appendScalar output (Num.toU32 byte)
            |> Result.withDefault "" # this will never fail
        else
            when byte is
                46 # '.'
                | 95 # '_'
                | 126 # '~'
                | 150 -> # '-'
                    # These special characters can all be unescaped in paths
                    Str.appendScalar output (Num.toU32 byte)
                    |> Result.withDefault "" # this will never fail

                _ ->
                    # This needs encoding in a path
                    suffix =
                        Str.toUtf8 percentEncoded
                        |> List.sublist { len: 3, start: 3 * Num.toNat byte }
                        |> Str.fromUtf8
                        |> Result.withDefault "" # This will never fail

                    Str.concat output suffix

## Adds a [Str] query parameter to the end of the [Url]. The key
## and value both get [percent-encoded](https://en.wikipedia.org/wiki/Percent-encoding).
##
##     Url.fromStr "https://example.com"
##         |> Url.appendParam "email" "someone@example.com"
##     # https://example.com?email=someone%40example.com
##
## This can be called multiple times on the same URL.
##
##     Url.fromStr "https://example.com"
##         |> Url.appendParam "café" "du Monde"
##         |> Url.appendParam "email" "hi@example.com"
##     # https://example.com?caf%C3%A9=du%20Monde&email=hi%40example.com
appendParam : Url, Str, Str -> Url
appendParam = \@Url urlStr, key, value ->
    { withoutFragment, afterQuery } =
        when Str.splitLast urlStr "#" is
            Ok { before, after } ->
                # The fragment is almost certainly going to be a small string,
                # so this interpolation should happen on the stack.
                { withoutFragment: before, afterQuery: "#\(after)" }

            Err NotFound ->
                { withoutFragment: urlStr, afterQuery: "" }

    encodedKey = percentEncode key
    encodedValue = percentEncode value

    bytes =
        Str.countUtf8Bytes withoutFragment
        + 1 # for "?" or "&"
        + Str.countUtf8Bytes encodedKey
        + 1 # for "="
        + Str.countUtf8Bytes encodedValue
        + Str.countUtf8Bytes afterQuery

    withoutFragment
    |> Str.reserve bytes
    |> Str.concat (if hasQuery (@Url withoutFragment) then "&" else "?")
    |> Str.concat encodedKey
    |> Str.concat "="
    |> Str.concat encodedValue
    |> Str.concat afterQuery
    |> @Url

## Replaces the URL's [query](https://en.wikipedia.org/wiki/URL#Syntax)—the part after
## the `?`, if it has one, but before any `#` it might have.
##
##     Url.fromStr "https://example.com?key1=val1&key2=val2#stuff"
##         |> Url.withQuery "newQuery=thisRightHere"
##     # https://example.com?newQuery=thisRightHere#stuff
##
## Passing `""` removes the `?` (if there was one).
##
##     Url.fromStr "https://example.com?key1=val1&key2=val2#stuff"
##         |> Url.withQuery ""
##     # https://example.com#stuff
withQuery : Url, Str -> Url
withQuery = \@Url urlStr, queryStr ->
    { withoutFragment, afterQuery } =
        when Str.splitLast urlStr "#" is
            Ok { before, after } ->
                # The fragment is almost certainly going to be a small string,
                # so this interpolation should happen on the stack.
                { withoutFragment: before, afterQuery: "#\(after)" }

            Err NotFound ->
                { withoutFragment: urlStr, afterQuery: "" }

    beforeQuery =
        when Str.splitLast withoutFragment "?" is
            Ok { before } -> before
            Err NotFound -> withoutFragment

    if Str.isEmpty queryStr then
        @Url (Str.concat beforeQuery afterQuery)
    else
        bytes =
            Str.countUtf8Bytes beforeQuery
            + 1 # for "?"
            + Str.countUtf8Bytes queryStr
            + Str.countUtf8Bytes afterQuery

        beforeQuery
        |> Str.reserve bytes
        |> Str.concat "?"
        |> Str.concat queryStr
        |> Str.concat afterQuery
        |> @Url

## Returns the URL's [query](https://en.wikipedia.org/wiki/URL#Syntax)—the part after
## the `?`, if it has one, but before any `#` it might have.
##
##     Url.fromStr "https://example.com?key1=val1&key2=val2&key3=val3#stuff"
##         |> Url.query
##     # "key1=val1&key2=val2&key3=val3"
##
## Returns `""` if the URL has no query.
##
##     Url.fromStr "https://example.com#stuff"
##         |> Url.query
##     # ""
query : Url -> Str
query = \@Url urlStr ->
    withoutFragment =
        when Str.splitLast urlStr "#" is
            Ok { before } -> before
            Err NotFound -> urlStr

    when Str.splitLast withoutFragment "?" is
        Ok { after } -> after
        Err NotFound -> ""

## Returns `Bool.true` if the URL has a `?` in it.
##
##     Url.fromStr "https://example.com?key=value#stuff"
##         |> Url.hasQuery
##     # Bool.true
##
##     Url.fromStr "https://example.com#stuff"
##         |> Url.hasQuery
##     # Bool.false
hasQuery : Url -> Bool
hasQuery = \@Url urlStr ->
    # TODO use Str.contains once it exists. It should have a "fast path"
    # with SIMD iteration if the string is small enough to fit in a SIMD register.
    Str.toUtf8 urlStr
    |> List.contains (Num.toU8 '?')

## Returns the URL's [fragment](https://en.wikipedia.org/wiki/URL#Syntax)—the part after
## the `#`, if it has one.
##
##     Url.fromStr "https://example.com#stuff"
##         |> Url.fragment
##     # "stuff"
##
## Returns `""` if the URL has no fragment.
##
##     Url.fromStr "https://example.com"
##         |> Url.fragment
##     # ""
fragment : Url -> Str
fragment = \@Url urlStr ->
    when Str.splitLast urlStr "#" is
        Ok { after } -> after
        Err NotFound -> ""

## Replaces the URL's [fragment](https://en.wikipedia.org/wiki/URL#Syntax).
##
##     Url.fromStr "https://example.com#stuff"
##         |> Url.withFragment "things"
##     # https://example.com#things
##
## If the URL didn't have a fragment, adds one.
##
##     Url.fromStr "https://example.com"
##         |> Url.withFragment "things"
##     # https://example.com#things
##
## Passing `""` removes the fragment.
##
##     Url.fromStr "https://example.com#stuff"
##         |> Url.withFragment ""
##     # https://example.com
withFragment : Url, Str -> Url
withFragment = \@Url urlStr, fragmentStr ->
    when Str.splitLast urlStr "#" is
        Ok { before } ->
            if Str.isEmpty fragmentStr then
                # If the given fragment is empty, remove the URL's fragment
                @Url before
            else
                # Replace the URL's old fragment with this one, discarding `after`
                @Url "\(before)#\(fragmentStr)"

        Err NotFound ->
            if Str.isEmpty fragmentStr then
                # If the given fragment is empty, leave the URL as having no fragment
                @Url urlStr
            else
                # The URL didn't have a fragment, so give it this one
                @Url "\(urlStr)#\(fragmentStr)"

## Returns `Bool.true` if the URL has a `#` in it.
##
##     Url.fromStr "https://example.com?key=value#stuff"
##         |> Url.hasFragment
##     # Bool.true
##
##     Url.fromStr "https://example.com?key=value"
##         |> Url.hasFragment
##     # Bool.false
hasFragment : Url -> Bool
hasFragment = \@Url urlStr ->
    # TODO use Str.contains once it exists. It should have a "fast path"
    # with SIMD iteration if the string is small enough to fit in a SIMD register.
    Str.toUtf8 urlStr
    |> List.contains (Num.toU8 '#')

strWithCapacity : Nat -> Str
strWithCapacity = \cap ->
    Str.reserve "" cap

# Adapted from the percent-encoding crate, © The rust-url developers, Apache2-licensed
#
# https://github.com/servo/rust-url/blob/e12d76a61add5bc09980599c738099feaacd1d0d/percent_encoding/src/lib.rs#L183
percentEncoded : Str
percentEncoded = "%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%20%21%22%23%24%25%26%27%28%29%2A%2B%2C%2D%2E%2F%30%31%32%33%34%35%36%37%38%39%3A%3B%3C%3D%3E%3F%40%41%42%43%44%45%46%47%48%49%4A%4B%4C%4D%4E%4F%50%51%52%53%54%55%56%57%58%59%5A%5B%5C%5D%5E%5F%60%61%62%63%64%65%66%67%68%69%6A%6B%6C%6D%6E%6F%70%71%72%73%74%75%76%77%78%79%7A%7B%7C%7D%7E%7F%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F%90%91%92%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9%AA%AB%AC%AD%AE%AF%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB%BC%BD%BE%BF%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE%CF%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF%F0%F1%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF"
