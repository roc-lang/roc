app [main!] { pf: platform "platform/main.roc" }

import Base64
import pf.Host

main! : {} => {}
main! = \{} ->
    when Base64.from_bytes(Str.to_utf8("Hello World")) is
        Err(_) -> Host.put_line!("sadness")
        Ok(encoded) ->
            Host.put_line!(Str.concat("encoded: ", encoded))

            when Base64.to_str(encoded) is
                Ok(decoded) -> Host.put_line!(Str.concat("decoded: ", decoded))
                Err(_) -> Host.put_line!("sadness")
