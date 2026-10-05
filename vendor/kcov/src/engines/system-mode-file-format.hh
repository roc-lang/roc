#pragma once

#include <stdint.h>
#include <string>
#include <sys/mman.h>

#include <utils.hh>

const uint32_t SYSTEM_MODE_FILE_MAGIC = 0x4d455247; // "MERG"
const uint32_t SYSTEM_MODE_FILE_VERSION = 1;

namespace kcov_system_mode
{
	struct system_mode_file
	{
		uint32_t magic;
		uint32_t version;
		uint32_t n_entries;
		uint32_t header_checksum;
		uint32_t filename_offset;
		uint32_t kcov_options_offset; // --include-pattern etc
		uint32_t data[];
	};

	class system_mode_memory
	{
	public:
		system_mode_memory(const std::string &fn, const std::string &opts, uint32_t n) :
			filename(fn),
			options(opts),
			n_entries(n)
		{
			mapped = true;
			data = (uint32_t *)::mmap(NULL, (n_entries + 2) * sizeof(uint32_t), PROT_READ | PROT_WRITE,
					MAP_SHARED | MAP_ANONYMOUS, -1, 0);
			if (!data)
			{
				// Fallback to plain malloc - won't work with forks
				data = (uint32_t *)xmalloc((n_entries + 2) * sizeof(uint32_t));
			}

			dirtyCount = &data[n_entries];
			cleanCount = &data[n_entries + 1];

			*dirtyCount = 0;
			*cleanCount = 0;
		}

		~system_mode_memory()
		{
			if (mapped)
			{
				munmap(data, n_entries * sizeof(uint32_t));
			}
			else
			{
				free(data);
			}
		}

		void reportIndex(uint32_t index);

		bool indexIsHit(uint32_t index) const;

		bool isDirty() const;

		void markClean();

		const std::string filename;
		const std::string options;
		const uint32_t n_entries;
		uint32_t *data;

		uint32_t *dirtyCount;
		uint32_t *cleanCount;

	private:
		bool mapped;
	};

	struct system_mode_file *memoryToFile(const class system_mode_memory &mem, size_t &outSize);

	class system_mode_memory *fileToMemory(const struct system_mode_file &file);

	class system_mode_memory *diskToMemory(const std::string &src);
};
