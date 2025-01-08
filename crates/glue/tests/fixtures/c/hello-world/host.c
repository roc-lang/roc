#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include "test_glue/roc_app.h"

int main(void)
{
    uint8_t main_for_host = roc_main_for_host();

    printf("main_for_host = %i\n", main_for_host);

    assert(main_for_host == 42);
}