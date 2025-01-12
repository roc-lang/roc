module [line!, raw!]

import pf.Host

line! : Str => ()
line! = \text ->
    Host.put_line!(text)

raw! : Str => ()
raw! = \text ->
    Host.put_raw!(text)
