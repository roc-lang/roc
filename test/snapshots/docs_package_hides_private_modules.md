# META
~~~ini
description=Issue 10077: package docs hide private mods and private type declarations
type=docs
~~~
# SOURCE
## main.roc
~~~roc
package [Date, Time] {}
~~~
## Date.roc
~~~roc
import Util

## A calendar date.
Date :: {}.{
    ## Date formatting options.
    Format :: {}
}

## Private implementation details.
Help :: {}
~~~
## Time.roc
~~~roc
## A time of day.
Time :: {}
~~~
## Util.roc
~~~roc
## Private utilities used by Date.
Util :: {}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "Date")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Date")
      (kind opaque)
      (type "Date :: " (record))
      (doc "A calendar date.")
      (entry
        (name "Format")
        (kind opaque)
        (type "Format :: " (record))
        (doc "Date formatting options.")
      )
    )
  )
  (mod
    (name "Time")
    (package "mod")
    (kind type_mod)
    (doc "A time of day.")
    (entry
      (name "Time")
      (kind opaque)
      (type "Time :: " (record))
      (doc "A time of day.")
    )
  )
)
~~~
