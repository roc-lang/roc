app [main] { pf: platform "platform/main.roc" }

import Base64
import pf.PlatformTask

IO a : Task a []

main : IO {}
main =
    when Base64.fromBytes (Str.toUtf8 "Hello World") is
        Err _ -> PlatformTask.putLine "sadness"
        Ok encoded ->
            PlatformTask.putLine! (Str.concat "encoded: " encoded)

            when Base64.toStr encoded is
                Ok decoded -> PlatformTask.putLine (Str.concat "decoded: " decoded)
                Err _ -> PlatformTask.putLine "sadness"
