# META
~~~ini
description=Platform docs hide internal modules not listed in exposes
type=docs
~~~
# SOURCE
## main.roc
~~~roc
platform ""
    requires {} { main! : () => {} }
    exposes [Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_line": Host.stdout_line!,
    }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

import Host
import Stdout

main_for_host! : () => {}
main_for_host! = main!
~~~
## Stdout.roc
~~~roc
import Host

## Public standard output helpers.
Stdout := [].{
    ## Write a string to standard output.
    line! : Str => {}
    line! = |message| Host.stdout_line!(message)
}
~~~
## Host.roc
~~~roc
## Internal host boundary.
Host := [].{
    ## Internal host effect.
    stdout_line! : Str => {}
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (module
    (name "Stdout")
    (package "app")
    (kind type_module)
    (entry
      (name "Stdout")
      (kind nominal)
      (type "Stdout := " (tag-union))
      (doc "Public standard output helpers.")
      (entry
        (name "line!")
        (kind value)
        (type (fn! (type-ref (name "Str")) (record)))
        (doc "Write a string to standard output.")
      )
    )
  )
)
~~~
