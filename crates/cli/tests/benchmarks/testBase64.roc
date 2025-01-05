app [main] { pf: platform "platform/main.roc" }

import Base64
import pf.PlatformTasks

IO a : Task a []

main : IO {}
main =
    when Base64.from_bytes(Str.to_utf8("Hello World")) is
        Err(_) -> PlatformTasks.put_line("sadness")
        Ok(encoded) ->
            PlatformTasks.put_line!(Str.concat("encoded: ", encoded))

            when Base64.to_str(encoded) is
                Ok(decoded) -> PlatformTasks.put_line(Str.concat("decoded: ", decoded))
                Err(_) -> PlatformTasks.put_line("sadness")
