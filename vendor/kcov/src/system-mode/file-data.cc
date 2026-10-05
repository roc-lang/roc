#include <system-mode/file-data.hh>

#include <utils.hh>

using namespace kcov;

static bool unmarshalTrailer(struct trailer *p)
{
	// Native-endian
	if (p->magic != TRAILER_MAGIC)
	{
		return false;
	}
	if (p->version != TRAILER_VERSION)
	{
		return false;
	}

	if ((p->filename_size & 7) != 0)
	{
		return false;
	}
	if ((p->options_size & 7) != 0)
	{
		return false;
	}

	return true;
}

static void marshalTrailer(struct trailer *p)
{
}

static size_t padTo8(size_t sz)
{
	if (sz & 7)
	{
		sz += 8 - (sz & 7);
	}

	return sz;
}

// Implement zlib compression later
static uint64_t *uncompressEntries(const void *data, uint32_t compressedSize, uint32_t uncompressedSize)
{
	uint64_t *out = (uint64_t *)xmalloc(uncompressedSize);

	memcpy(out, data, compressedSize);

	return out;
}

static void *compressEntries(uint32_t &outCompressedSize, const uint64_t *data, uint32_t dataSize)
{
	void *out = xmalloc(dataSize);

	outCompressedSize = dataSize;
	memcpy(out, data, dataSize);

	return out;
}


SystemModeFile::SystemModeFile() :
		m_id(0),
		m_data(NULL),
		m_fileSize(0)
{
}

SystemModeFile::~SystemModeFile()
{
	free(m_data);
}


SystemModeFile *SystemModeFile::fromRawFile(uint32_t id, const std::string &filename, const std::string &options,
		const void *fileData, size_t fileSize)
{
	SystemModeFile *out = new SystemModeFile();

	bool rv = out->parse(fileData, fileSize, true);

	if (!rv)
	{
		delete out;
		return NULL;
	}

	out->m_id = id;
	out->m_options = options;
	out->m_filename = filename;

	return out;
}

SystemModeFile *SystemModeFile::fromProcessedFile(const void *fileData, size_t fileSize)
{
	SystemModeFile *out = new SystemModeFile();

	bool rv = out->parse(fileData, fileSize, false);

	if (!rv)
	{
		delete out;
		return NULL;
	}

	return out;
}

bool SystemModeFile::parse(const void *fileData, size_t fileSize, bool isRaw)
{
	if (!fileData)
	{
		return false;
	}

	if (fileSize == 0)
	{
		return false;
	}

	m_data = xmalloc(fileSize);
	memcpy(m_data, fileData, fileSize);
	m_fileSize = fileSize;

	if (isRaw)
	{
		// All set
		return true;
	}

	uint8_t *p = ( uint8_t *)fileData;
	uint8_t *trailerStart = (p + fileSize - sizeof(struct trailer));
	struct trailer *trailer = (struct trailer *)trailerStart;

	// Trailer and data must be 8-byte aligned
	if (((unsigned long)trailer) % 8 != 0)
	{
		return false;
	}

	if (!unmarshalTrailer(trailer))
	{
		return false;
	}

	if (trailer->filename_size + trailer->options_size + trailer->compressed_size > fileSize + sizeof(*trailer))
	{
		return false;
	}


	const char *filename = (const char *)(trailerStart - trailer->compressed_size - trailer->options_size - trailer->filename_size);
	if (filename[trailer->filename_size - 1] != '\0')
	{
		return false;
	}

	const char *options = (const char *)(trailerStart - trailer->compressed_size - trailer->options_size);
	if (options[trailer->options_size - 1] != '\0')
	{
		return false;
	}
	m_filename = filename;
	m_options = options;
	m_id = trailer->id;

	uint64_t *entries = uncompressEntries((void *)(trailerStart - trailer->compressed_size), trailer->compressed_size, trailer->uncompressed_size);
	if (!entries)
	{
		return false;
	}

	m_entries.clear();
	m_entries.reserve(trailer->n_entries);
	for (unsigned i = 0; i < trailer->n_entries; i++)
	{
		addEntry(i, entries[i]);
	}
	free(entries);

	return true;
}

void SystemModeFile::addEntry(uint32_t index, uint64_t value)
{
	if (m_entries.size() <= index)
	{
		m_entries.resize(index + 1);
	}

	m_entries[index] = value;
}

const std::vector<uint64_t> &SystemModeFile::getEntries() const
{
	return m_entries;
}

uint32_t SystemModeFile::getId() const
{
	return m_id;
}

const std::string &SystemModeFile::getOptions() const
{
	return m_options;
}

const std::string &SystemModeFile::getFilename() const
{
	return m_filename;
}

const void *SystemModeFile::getProcessedData(size_t &outSize) const
{
	outSize = m_fileSize;

	if (m_entries.size() == 0)
	{
		uint8_t *out = (uint8_t*)xmalloc(outSize);

		memcpy(out, m_data, m_fileSize);
		return out;
	}

	size_t padded = padTo8(outSize);

	uint32_t filenameSize;
	uint32_t optionsSize;
	uint32_t uncompressedSize = m_entries.size() * sizeof(uint64_t);
	uint32_t compressedSize;
	void *compressed = compressEntries(compressedSize, m_entries.data(), uncompressedSize);
	if (!compressed)
	{
		return NULL;
	}
	compressedSize = padTo8(compressedSize);

	filenameSize = padTo8(m_filename.size() + 1);
	optionsSize = padTo8(m_options.size() + 1);

	outSize = padded + filenameSize + optionsSize + compressedSize + sizeof(struct trailer);
	uint8_t *out = (uint8_t*)xmalloc(outSize);

	memcpy(out, m_data, m_fileSize);

	uint8_t *start = out + padded;
	char *filename = (char *)start;
	char *options = (char *)start + filenameSize;
	uint8_t *trailerStart = out + outSize - sizeof(trailer);
	struct trailer *trailer = (struct trailer *)trailerStart;

	strcpy(filename, m_filename.c_str());
	options[filenameSize - 1] = '\0';

	strcpy(options, m_options.c_str());
	options[optionsSize - 1] = '\0';
	memcpy(start + filenameSize + optionsSize, m_entries.data(), m_entries.size() * sizeof(uint64_t));

	trailer->magic = TRAILER_MAGIC;
	trailer->version = TRAILER_VERSION;
	trailer->n_entries = m_entries.size();
	trailer->uncompressed_size = uncompressedSize;
	trailer->compressed_size = compressedSize;
	trailer->filename_size = filenameSize;
	trailer->options_size = optionsSize;
	trailer->id = m_id;

	marshalTrailer(trailer);

	// Not yet implemented
	free(compressed);

	return out;
}
