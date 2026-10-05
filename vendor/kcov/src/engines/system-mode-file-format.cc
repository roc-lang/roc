#include "system-mode-file-format.hh"

#include <utils.hh>

struct kcov_system_mode::system_mode_file *kcov_system_mode::memoryToFile(const class kcov_system_mode::system_mode_memory &mem, size_t &outSize)
{
	outSize = sizeof(struct system_mode_file) + mem.n_entries * sizeof(uint32_t) +
			mem.filename.size() + mem.options.size() + 2;
	struct system_mode_file *out = (struct system_mode_file *)xmalloc(outSize);
	char *p = (char *)out;

	out->magic = SYSTEM_MODE_FILE_MAGIC;
	out->version = SYSTEM_MODE_FILE_VERSION;
	out->n_entries = mem.n_entries;
	out->filename_offset = mem.n_entries * sizeof(uint32_t) + sizeof(*out);
	out->kcov_options_offset = out->filename_offset + mem.filename.size() + 1;

	strcpy(&p[out->filename_offset], mem.filename.c_str());
	strcpy(&p[out->kcov_options_offset], mem.options.c_str());

	memcpy(out->data, mem.data, out->n_entries * sizeof(uint32_t));

	return out;
}

class kcov_system_mode::system_mode_memory *kcov_system_mode::fileToMemory(const struct kcov_system_mode::system_mode_file &file)
{
	if (file.magic != SYSTEM_MODE_FILE_MAGIC)
	{
		return NULL;
	}

	if (file.version != SYSTEM_MODE_FILE_VERSION)
	{
		return NULL;
	}

	size_t sizeExceptStrings = sizeof(file) + file.n_entries * sizeof(uint32_t);

	if (file.filename_offset < sizeExceptStrings)
	{
		return NULL;
	}

	if (file.kcov_options_offset < file.filename_offset + 1)
	{
		return NULL;
	}
	char *p = (char *)&file;

	kcov_system_mode::system_mode_memory *out = new kcov_system_mode::system_mode_memory(std::string(&p[file.filename_offset]),
			std::string(&p[file.kcov_options_offset]),
			file.n_entries);

	memcpy(out->data, file.data, out->n_entries * sizeof(uint32_t));

	return out;
}

class kcov_system_mode::system_mode_memory *kcov_system_mode::diskToMemory(const std::string &src)
{
	size_t sz;
	kcov_system_mode::system_mode_file *p = (kcov_system_mode::system_mode_file *)read_file(&sz, "%s", src.c_str());

	if (!p)
	{
		return NULL;
	}

	kcov_system_mode::system_mode_memory *out = kcov_system_mode::fileToMemory(*p);
	free(p);

	return out;
}

bool kcov_system_mode::system_mode_memory::indexIsHit(uint32_t index) const
{
	if (index / 32 >= n_entries)
	{
		return false;
	}

	uint32_t cur = data[index / 32];
	uint32_t bit = index % 32;

	return !!(cur & (1 << bit));
}

void kcov_system_mode::system_mode_memory::reportIndex(uint32_t index)
{
	unsigned int wordIdx = index / 32;
	unsigned int bit = index % 32;

	if (wordIdx >= n_entries)
	{
		return;
	}

	// Update the bit atomically
	uint32_t *p = &data[wordIdx];

	// Already hit?
	if (*p & (1 << bit))
		return;

	uint32_t val, newVal;
	do
	{
		val = *p;
		newVal = val | (1 << bit);

	} while (!__sync_bool_compare_and_swap(p, val, newVal));

	__sync_fetch_and_add(dirtyCount, 1);
}

bool kcov_system_mode::system_mode_memory::isDirty() const
{
	return *dirtyCount != *cleanCount;
}

void kcov_system_mode::system_mode_memory::markClean()
{
	*cleanCount = *dirtyCount;
}
