
```ruby
module Equatable(val) {
    equals : val, val -> Bool
}

Equatable.equals(foo)

|bytes| module([a.decode : _ -> Result(a, _)]).decode(bytes)

|bytes| module({ decode : List U8 -> Result(val, _) }).decode(bytes)

|bytes| module(val.{ decode : List U8 -> Result(val, DecodeErr) }).decode(bytes)

Decoding implements
    decoder : Decoder val fmt
        where val implements Decoding, fmt implements DecoderFormatting

```
