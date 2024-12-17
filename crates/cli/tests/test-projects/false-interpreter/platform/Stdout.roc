module [line!, raw!]

import pf.Host

line! : Str => {}
line! = \text ->
    Host.putLine! text

raw! : Str => {}
raw! = \text ->
    Host.putRaw! text
