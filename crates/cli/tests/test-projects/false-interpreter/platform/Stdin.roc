module [line!, char!]

import pf.Host

line! : {} => Str
line! = \{} ->
    Host.getLine! {}

char! : {} => U8
char! = \{} ->
    Host.getChar! {}
