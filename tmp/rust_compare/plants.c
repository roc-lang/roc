#include <stdlib.h>
#include <stdint.h>

typedef struct { int32_t x; uint32_t type; } Plant;

extern Plant random_plant(int32_t seed);

typedef struct {
    size_t cap;
    Plant *ptr;
    size_t len;
} PlantVec;

// Matches Roc's RocOps.roc_crashed ABI: (const RocCrashed *, void *env), noreturn.
typedef struct { uint8_t *utf8_bytes; size_t len; } RocCrashed;
extern _Noreturn void handle_oom(const RocCrashed *crashed, void *env);

#define PLANT_COUNT 15

PlantVec starting_plants(void) {
    Plant *ptr = malloc(PLANT_COUNT * sizeof(Plant));
    if (ptr == NULL) {
        static const char msg[] = "Ran out of memory!";
        RocCrashed crashed = { (uint8_t *)msg, sizeof(msg) - 1 };
        handle_oom(&crashed, NULL);
    }
    for (int i = 0; i < PLANT_COUNT; i++) {
        ptr[i] = random_plant(i * 12);
    }
    return (PlantVec){ .ptr = ptr, .len = PLANT_COUNT, .cap = PLANT_COUNT };
}
