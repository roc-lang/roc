#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include "test_glue/roc_app.h"

int main(void)
{
    uint8_t mainForHost = roc_mainForHost();

    printf("mainForHost = %i\n", mainForHost);

    assert(mainForHost == 42);
}