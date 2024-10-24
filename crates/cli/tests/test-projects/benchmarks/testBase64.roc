app [main] { pf: platform "platform/main.roc" }

import Base64
import pf.PlatformTasks

IO a : Task a []

main : IO {}
main =
    when Base64.fromBytes (Str.toUtf8 "Hello World") is
        Err _ -> PlatformTasks.putLine "sadness"
        Ok encoded ->
            PlatformTasks.putLine! (Str.concat "encoded: " encoded)

            when Base64.toStr encoded is
                Ok decoded -> PlatformTasks.putLine (Str.concat "decoded: " decoded)
                Err _ -> PlatformTasks.putLine "sadness"
