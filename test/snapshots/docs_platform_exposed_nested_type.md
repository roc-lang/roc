# META
~~~ini
description=Platform docs expose a selected nested type without its source parent
type=docs
~~~
# SOURCE
## main.roc
~~~roc
platform "blub"
    requires {}
    exposes [Container.Blub]
    packages {}
    provides {}
    targets: {}

import Container
~~~
## Container.roc
~~~roc
Container :: [].{
    Blub :: [].{}
}
~~~
# DOCS
~~~clojure
(package-docs
  (name "test-app")
  (mod
    (name "Blub")
    (package "mod")
    (kind type_mod)
    (entry
      (name "Blub")
      (kind opaque)
      (type "Blub :: " (tag-union))
    )
  )
)
~~~
