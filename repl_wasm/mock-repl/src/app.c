#include <stddef.h>
#include "byte_array.h"

// The starting value of the countdown. We will modify this with the "compiler".
// It's not a very good compiler, all it can do is change this number.
int start_value = 22;

char result_buffer[260];

ByteArray *run()
{
    ByteArray *result = (ByteArray*) result_buffer;

    size_t i = 0;
    for (char x = start_value; x; --x)
    {
        result->bytes[i++] = x;
    }
    result->length = start_value;

    return result;
}
