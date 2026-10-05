#pragma once

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

struct phdr_data_segment
{
	unsigned long paddr;
	unsigned long vaddr;
	unsigned long size;
};

struct phdr_data_entry
{
	char name[1024];
	uint32_t n_segments;

	// "Reasonable" max
	struct phdr_data_segment segments[64];
};

struct phdr_data
{
	uint32_t magic;
	uint32_t version;
	unsigned long relocation; // for PIE
	uint32_t n_entries;

	struct phdr_data_entry entries[];
};

struct dl_phdr_info;

struct phdr_data *phdr_data_new(size_t size);

void phdr_data_add(struct phdr_data *p, struct dl_phdr_info *info);

void *phdr_data_marshal(struct phdr_data *p, size_t *out_sz);

void phdr_data_free(struct phdr_data *p);

struct phdr_data *phdr_data_unmarshal(void *p);

#ifdef __cplusplus
}
#endif
