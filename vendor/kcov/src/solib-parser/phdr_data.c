#ifndef _GNU_SOURCE
# define _GNU_SOURCE
#endif
#include "phdr_data.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <link.h>

#define KCOV_MAGIC         0x6b636f76 /* "kcov" */
#define KCOV_SOLIB_VERSION 3

// Add symbols missing from FreeBSD's elfutils
#ifndef __ELF_NATIVE_CLASS
#ifdef __LP64__
#define __ELF_NATIVE_CLASS 64
#else
#define __ELF_NATIVE_CLASS 32
#endif
#endif
#ifndef ElfW
// Copied from glibc's elf/link.h
#define ElfW(type)	_ElfW (Elf, __ELF_NATIVE_CLASS, type)
#define _ElfW(e,w,t)	_ElfW_1 (e, w, _##t)
#define _ElfW_1(e,w,t) e##w##t
#endif

static uint8_t data_area[4 * 1024 * 1024];

struct phdr_data *phdr_data_new(size_t allocSize)
{
	struct phdr_data *p = (struct phdr_data *)data_area;

	if (allocSize > sizeof(data_area))
		return NULL;

	p->magic = KCOV_MAGIC;
	p->version = KCOV_SOLIB_VERSION;
	p->relocation = 0;
	p->n_entries = 0;

	return p;
}

void phdr_data_free(struct phdr_data *p)
{
}


void phdr_data_add(struct phdr_data *p, struct dl_phdr_info *info)
{
	uint32_t n = p->n_entries + 1;
	uint32_t curEntry = p->n_entries;
	int phdr;

	struct phdr_data_entry *cur = &p->entries[curEntry];

	memset(cur, 0, sizeof(*cur));
	strncpy(cur->name, info->dlpi_name, sizeof(cur->name) - 1);

	cur->n_segments = 0;
	for (phdr = 0; phdr < info->dlpi_phnum; phdr++)
	{
		const ElfW(Phdr) *curHdr = &info->dlpi_phdr[phdr];
		struct phdr_data_segment *seg = &cur->segments[cur->n_segments];

		if (curHdr->p_type != PT_LOAD)
			continue;

		if (cur->n_segments >= sizeof(cur->segments) / sizeof(cur->segments[0]))
		{
			fprintf(stderr, "Too many segments\n");
			return;
		}

		seg->paddr = curHdr->p_paddr;
		seg->vaddr = info->dlpi_addr + curHdr->p_vaddr;
		seg->size = curHdr->p_memsz;

		cur->n_segments++;
	}

	p->n_entries = n;
}

void *phdr_data_marshal(struct phdr_data *p, size_t *out_sz)
{
	*out_sz = sizeof(struct phdr_data) + sizeof(struct phdr_data_entry) * p->n_entries;

	return (void *)p;
}

struct phdr_data *phdr_data_unmarshal(void *p)
{
	struct phdr_data *out = (struct phdr_data *)p;

	if (out->magic != KCOV_MAGIC)
		return NULL;
	if (out->version != KCOV_SOLIB_VERSION)
		return NULL;
	// Silly amounts of entries mean broken data
	if (out->n_entries > 0x10000)
		return NULL;

	return out;
}
