app [makeGlue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
# import pf.Shape exposing [Shape, RocFn]
import pf.File exposing [File]
# import pf.TypeId exposing [TypeId]

makeGlue : List Types -> Result (List File) Str
makeGlue = \_typesByArch ->
    Ok ([{ name: "roc_app.h", content: placeholderGlue }])

placeholderGlue =
    """
    #ifndef ROC_APP_H
    #define ROC_APP_H

    #include <stdint.h>

    extern void roc__mainForHost_1_exposed_generic(uint8_t *ret);

    uint8_t roc_mainForHost()
    {
        uint8_t ret;

        roc__mainForHost_1_exposed_generic(&ret);

        return ret;
    }

    #endif
    """
