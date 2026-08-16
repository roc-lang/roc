platform "glue-return-list-elements"
    requires {}
    exposes []
    packages {}
    provides {
        "roc_labels": labels_for_host,
    }
    targets: {}

labels_for_host : {} -> List(Str)
labels_for_host = |_| ["one", "two"]
