# META
~~~ini
description=Issue 10077: package docs hide private modules and private type declarations
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
Date :: {}

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
  (module
    (name "Date")
    (package "module")
    (kind type_module)
    (entry
      (name "Date")
      (kind opaque)
      (type "Date :: " (record))
      (doc "A calendar date.")
    )
  )
  (module
    (name "Time")
    (package "module")
    (kind type_module)
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
