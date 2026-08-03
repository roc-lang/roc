// Keep Binaryen C++ API use contained behind a C ABI boundary.

#include "binaryen-c.h"

#include <cstdint>
#include <cstdlib>
#include <mutex>

extern "C" {

typedef struct RocBinaryenOptimizeConfig {
    int optimize_level;
    int shrink_level;
    uint8_t zero_filled_memory;
    uint8_t debug_info;
    uint8_t strip_debug;
    uint8_t strip_producers;
    uint8_t strip_target_features;
    uint8_t validate;
} RocBinaryenOptimizeConfig;

typedef struct RocBinaryenBuffer {
    uint8_t* ptr;
    size_t len;
} RocBinaryenBuffer;

enum RocBinaryenStatus {
    RocBinaryenStatusOk = 0,
    RocBinaryenStatusInvalidArguments = 1,
    RocBinaryenStatusReadFailed = 2,
    RocBinaryenStatusValidateFailed = 3,
    RocBinaryenStatusWriteFailed = 4,
};

}

namespace {

std::mutex binaryen_mutex;

struct PassOptionsGuard {
    int optimize_level;
    int shrink_level;
    bool zero_filled_memory;
    bool debug_info;

    PassOptionsGuard()
        : optimize_level(BinaryenGetOptimizeLevel()),
          shrink_level(BinaryenGetShrinkLevel()),
          zero_filled_memory(BinaryenGetZeroFilledMemory()),
          debug_info(BinaryenGetDebugInfo()) {}

    ~PassOptionsGuard() {
        BinaryenSetOptimizeLevel(optimize_level);
        BinaryenSetShrinkLevel(shrink_level);
        BinaryenSetZeroFilledMemory(zero_filled_memory);
        BinaryenSetDebugInfo(debug_info);
    }
};

} // namespace

extern "C" int RocBinaryenOptimizeWasm(
    const uint8_t* input,
    size_t input_len,
    RocBinaryenOptimizeConfig config,
    RocBinaryenBuffer* output
) {
    if (input == nullptr || input_len == 0 || output == nullptr) {
        return RocBinaryenStatusInvalidArguments;
    }

    output->ptr = nullptr;
    output->len = 0;

    std::lock_guard<std::mutex> lock(binaryen_mutex);
    PassOptionsGuard guard;

    BinaryenSetOptimizeLevel(config.optimize_level);
    BinaryenSetShrinkLevel(config.shrink_level);
    BinaryenSetZeroFilledMemory(config.zero_filled_memory != 0);
    BinaryenSetDebugInfo(config.debug_info != 0);

    BinaryenModuleRef module = BinaryenModuleReadWithFeatures(
        reinterpret_cast<char*>(const_cast<uint8_t*>(input)),
        input_len,
        BinaryenFeatureAll()
    );
    if (module == nullptr) {
        return RocBinaryenStatusReadFailed;
    }

    BinaryenModuleOptimize(module);

    const char* strip_passes[4];
    BinaryenIndex strip_count = 0;
    if (config.strip_debug != 0 && config.debug_info == 0) {
        strip_passes[strip_count++] = "strip-debug";
        strip_passes[strip_count++] = "strip-dwarf";
    }
    if (config.strip_producers != 0) {
        strip_passes[strip_count++] = "strip-producers";
    }
    if (config.strip_target_features != 0) {
        strip_passes[strip_count++] = "strip-target-features";
    }
    if (strip_count != 0) {
        BinaryenModuleRunPasses(module, strip_passes, strip_count);
    }

    if (config.validate != 0 && !BinaryenModuleValidate(module)) {
        BinaryenModuleDispose(module);
        return RocBinaryenStatusValidateFailed;
    }

    BinaryenModuleAllocateAndWriteResult result =
        BinaryenModuleAllocateAndWrite(module, nullptr);
    BinaryenModuleDispose(module);

    if (result.sourceMap != nullptr) {
        free(result.sourceMap);
    }
    if (result.binary == nullptr && result.binaryBytes != 0) {
        return RocBinaryenStatusWriteFailed;
    }

    output->ptr = static_cast<uint8_t*>(result.binary);
    output->len = result.binaryBytes;
    return RocBinaryenStatusOk;
}

extern "C" void RocBinaryenFree(void* ptr) {
    free(ptr);
}
