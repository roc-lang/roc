fillBucketsFromData = \buckets0, data, shifts ->
    buckets1, (key, _), dataIndex <- List.walkWithIndex data buckets0
    (bucketIndex, distAndFingerprint) = nextWhileLess buckets1 key shifts
    placeAndShiftUp buckets1 { distAndFingerprint, dataIndex: Num.toU32 dataIndex } bucketIndex

foo
