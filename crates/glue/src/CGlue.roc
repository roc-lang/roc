app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
# import pf.Shape exposing [Shape, RocFn]
import pf.File exposing [File]
# import pf.TypeId exposing [TypeId]

## generate placeholder glue for now that only works for our one C test
make_glue : List Types -> Result (List File) Str
make_glue = \_types_by_arch ->
    Ok([{ name: "roc_app.h", content: placeholder_glue }])

placeholder_glue =
    """
    #ifndef ROC_APP_H
    #define ROC_APP_H

    #include <stdint.h>

    extern void roc__main_for_host_1_exposed_generic(uint8_t *ret);

    uint8_t roc_main_for_host()
    {
        uint8_t ret;

        roc__main_for_host_1_exposed_generic(&ret);

        return ret;
    }

    #endif
    """
