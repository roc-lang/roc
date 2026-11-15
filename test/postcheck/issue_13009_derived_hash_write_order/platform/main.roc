platform ""
    requires {} { probe : Hasher -> { derived : Hasher, manual : Hasher, reordered : Hasher } }
    exposes []
    packages {}
    provides { "roc_probe": probe_for_host }
    hosted {}
    targets: {}

probe_for_host : Hasher -> { derived : Hasher, manual : Hasher, reordered : Hasher }
probe_for_host = |hasher| probe(hasher)
