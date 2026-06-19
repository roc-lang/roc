package [] {}

# Minimal form of issue 9691: an unannotated (generalized) helper returning a
# tag union (`Try`) is matched inside an `expect`. The `Ok` arm is obviously the
# one that should be taken, but the dev backend specializes the test-root's
# tag-union return with the wrong discriminant and takes `Err` instead.
g : F64 -> Bool
g = |x| {
    to_int = |f| { (f * 1e6).to_i64_try() }

    match to_int(x) {
        Ok(_) => Bool.True
        Err(_) => Bool.False
    }
}

expect g(3)
