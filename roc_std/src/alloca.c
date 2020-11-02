#include <alloca.h>

// From https://github.com/TheDan64/scoped_alloca
// by Daniel Kolsoi, licensed under the Apache License 2.0
// Thank you, Dan!

void *c_alloca(size_t bytes) {
    return alloca(bytes);
}
