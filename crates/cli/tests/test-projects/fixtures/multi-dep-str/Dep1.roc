module [str1]

import Dep2

str1 : Str
str1 = Dep2.str2

str2 =
    if Bool.true then str1 else str1

str3 =
    str2

favorites = artists
    .keep_if(.is_favorite)
    .map(|artist| "★ ${artist.name}")

favorites =
    File.read!("artists.json", Json.utf8)?
    .keep_if(.is_favorite)
    .map(|artist| "★ ${artist.name}")
