#pragma once

#include <stdint.h>

#include <string>

struct new_process_entry
{
	uint32_t magic;
	uint32_t version;

	uint16_t entry_size;
	uint16_t pid;

	char filename[];
};

const static uint32_t NEW_PROCES_MAGIC = 0x76817812;
const static uint32_t NEW_PROCES_VERSION = 1;

bool parseProcessEntry(struct new_process_entry *p, uint16_t &outPid, std::string &outFilename);

struct new_process_entry *createProcessEntry(uint16_t pid, const std::string &filename);
