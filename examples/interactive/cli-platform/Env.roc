args : Task (List Str) [NonUnicodeArg (List U8)]* [Read [Args]*]*

argsAsBytes : Task (List (List U8)) [NonUnicodeArg (List U8)]* [Read [Args]*]*

walkArgs :

walkArgsUntil :

walkArgsAsBytes :

walkArgsAsBytesUntil :

## Environment variable entries may not be valid Unicode. If this environment variable
## is not valid unicode, the task fails with `InvalidUnicodeVar` and the raw bytes
## that were not valid unicode.
var :
    Str
    -> Task
        Str
        [VarNotFound Str, NonUnicodeVar (List U8)]*
        [Read [Env]*]*

## Environment variable keys and values may not be valid Unicode. This returns the raw bytes
## associated with the given [Str] key.
##
## To find an environment variable whose key is not a valid [Str], use [walkVarsUntil].
varBytes :
    Str
    -> Task
        (List U8)
        [VarNotFound Str]*
        [Read [Env]*]*

walkVars :
    state,
    (state, (List U8, List U8) -> Task state err fx)
    -> Task state err [Read [Env]*]fx

walkVarsUntil :
    state,
    (state, (List U8, List U8) -> Task [Done, Continue state] err fx)
    -> Task state err [Read [Env]*]fx

## Get the system's current [Locale].
##
## The first time this task runs, it reads the locale from the environment and
## translates it into a [Locale]. However, since the operating system sets the system's
## locale when the process begins, and does not change it afterwards (changing the system-wide
## locale typically requires a reboot to fully take effect), calling this multiple
## times results in a cached answer being provided after the first time.
##
## This means the first time this task runs, it will do some work to look up the
## environment setting, and create the [Locale] structure from that. Every time you call it after
## the first will be much faster, as it will return the cached value instead of redoing that work.
locale : Task Locale * [Read [Env]*]*
# Windows: https://docs.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-getthreadlocale
# UNIX: setlocale [sic] https://www.man7.org/linux/man-pages/man3/setlocale.3.html
#       yes, really - to *get* the locale, you call setlocale passing NULL

## Get the system's current [Charset].
##
## This is the same as calling [locale] and passing that [Locale] to [Locale.charset].
charset : Task Charset * [Read [Env]*]*

# Note: setLocale seems like a mistake, because on Windows this can only be done on
# a per-thread basis, not a per-process basis.
# https://docs.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-setthreadlocale

# TODO: should we allow Env.set? I don't want to. Seems like it could mess up locale,
# for example. We could disallow setting certain ones, but that seems like a mistake.
# Also, env vars are literally global mutable variables, so...why are you setting them?
# There's always a workaround involving passing values around!
# Disallowing setEnv guarantees (assuming the host cooperates) that locale won't change.
