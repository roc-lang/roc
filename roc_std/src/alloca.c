#include <alloca.h>

// Adapted from https://github.com/TheDan64/scoped_alloca
// by Daniel Kolsoi - license information can be found in
// the COPYRIGHT_DETAILS file in the root directory of this distribution.
//
// Thank you, Dan!

void *c_alloca(size_t bytes) {
    return alloca(bytes);
}
