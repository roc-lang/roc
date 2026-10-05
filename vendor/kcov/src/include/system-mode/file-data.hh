#pragma once

#include <stdint.h>
#include <stdlib.h>

#include <string>
#include <vector>

/* Data is always 8-byte aligned, and the trailer as well */
struct trailer
{
	uint32_t magic;
	uint32_t version;
	uint32_t compressed_size;
	uint32_t uncompressed_size;
	uint32_t id;

	uint32_t filename_size; // NULL-terminated, 8-byte aligned string
	uint32_t options_size;  // Ditto, after filename

	uint16_t n_entries;
};

const static uint32_t TRAILER_MAGIC = 0x19992000;
const static uint32_t TRAILER_VERSION = 1;

namespace kcov
{
	class SystemModeFile
	{
	public:
		~SystemModeFile();

		void addEntry(uint32_t index, uint64_t value);

		/**
		 * Get processed data, allocated with malloc.
		 */
		const void *getProcessedData(size_t &outSize) const;

		const std::string &getOptions() const;

		uint32_t getId() const;

		const std::string &getFilename() const;

		const std::vector<uint64_t> &getEntries() const;

		static SystemModeFile *fromRawFile(uint32_t id, const std::string &filename, const std::string &options,
				const void *fileData, size_t fileSize);
		static SystemModeFile *fromProcessedFile(const void *fileData, size_t fileSize);

	private:
		SystemModeFile();

		bool parse(const void *fileData, size_t fileSize, bool isRaw);

		std::string m_filename;
		std::string m_options;
		uint32_t m_id;
		void *m_data;
		size_t m_fileSize;

		std::vector<uint64_t> m_entries;
	};
};
