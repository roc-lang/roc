# META
~~~ini
description=Issue 10668: platform docs include an exposed type alias
type=docs
~~~
# SOURCE
## main.roc
~~~roc
platform "blub"
    requires {}
    exposes [Blub]
    packages {}
    provides {}
    targets: {}

import Container

Blub : Container.Blub

Private : {}
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
    (name "main")
    (package "mod")
    (kind platform)
    (entry
      (name "Blub")
      (kind alias)
      (type "Blub : " (type-ref (mod "Container") (name "Container.Blub")))
    )
  )
)
~~~
