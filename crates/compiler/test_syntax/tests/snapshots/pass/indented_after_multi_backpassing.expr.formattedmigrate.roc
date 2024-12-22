fill_buckets_from_data = \buckets0, data, shifts ->
    buckets1, (key, _), data_index <- List.walk_with_index data buckets0
    (bucket_index, dist_and_fingerprint) = next_while_less buckets1 key shifts
    place_and_shift_up buckets1 { dist_and_fingerprint, data_index: Num.to_u32 data_index } bucket_index

foo