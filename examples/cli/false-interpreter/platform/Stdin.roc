module [char]

import pf.PlatformTask

line : Task Str *
line = PlatformTask.getLine

char : Task U8 *
char = PlatformTask.getChar
