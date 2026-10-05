#include <source-file-cache.hh>
#include <utils.hh>

#include <unordered_map>

using namespace kcov;

class SourceFileCache : public ISourceFileCache
{
public:
	SourceFileCache() :
		m_empty()
	{
	}

	const std::vector<std::string> &getLines(const std::string &filePath)
	{
		const File &file = lookupFile(filePath);

		return file.m_lines;
	}

	bool fileExists(const std::string &filePath)
	{
		const File &file = lookupFile(filePath);

		return !file.m_lines.empty();
	}

	uint32_t getCrc(const std::string &filePath)
	{
		const File &file = lookupFile(filePath);

		return file.m_crc;
	}

private:
	class File
	{
	public:
		File() :
			m_data(NULL),
			m_dataSize(0),
			m_crc(0)
		{
		}

		~File()
		{
			free((void*)m_data);
		}

		File(const uint8_t *data, size_t size) :
			m_data(data), m_dataSize(size)
		{
			std::string fileData((const char*)m_data, size);

			m_crc = hash_block(data, size);

			m_lines = split_string(fileData, "\n");
		}

		const uint8_t *m_data;
		size_t m_dataSize;
		std::vector<std::string> m_lines;
		uint32_t m_crc;
	};

	const File &lookupFile(const std::string filePath)
	{
		std::unordered_map<std::string, File *>::iterator it = m_files.find(filePath);

		if (it != m_files.end())
			return *it->second;

		/* Doesn't exist - put it as empty in the cache */
		if (!file_exists(filePath))
		{
			m_files[filePath] = &m_empty;

			return m_empty;
		}

		size_t sz;
		uint8_t *p = (uint8_t *)read_file(&sz, "%s", filePath.c_str());

		// Can read?
		if (p)
			m_files[filePath] = new File(p, sz);
		else // Unreadable, populate with empty
			m_files[filePath] = &m_empty;

		return *m_files[filePath];
	}

	File m_empty;

	/* Pointer to avoid copies when populating the map. Move semantics
	 * would be better, but is >= C++11.
	 */
	std::unordered_map<std::string, File *> m_files;
};

ISourceFileCache &ISourceFileCache::getInstance()
{
	static SourceFileCache *g_instance;

	if (!g_instance)
		g_instance = new SourceFileCache();

	return *g_instance;
}
