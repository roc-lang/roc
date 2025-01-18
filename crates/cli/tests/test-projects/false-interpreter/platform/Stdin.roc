module [line!, char!]

import pf.Host

line! : () => Str
line! = \{} ->
    Host.get_line!()

char! : () => U8
char! = \{} ->
    Host.get_char!()
