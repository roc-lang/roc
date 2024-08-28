module [
    line,
    char,
]

import pf.PlatformTasks

line : Task Str *
line = PlatformTasks.getLine

char : Task U8 *
char = PlatformTasks.getChar
