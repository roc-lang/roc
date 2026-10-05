#include <file-parser.hh>
#include <collector.hh>
#include <utils.hh>
#include <filter.hh>
#include <writer.hh>
#include <configuration.hh>

#include <vector>
#include <string>
#include <list>
#include <unordered_map>
#include <map>

#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

#include <swap-endian.hh>

#include "merge-parser.hh"

using namespace kcov;

#define MERGE_MAGIC   0x4d6f6172 // "Moar"
#define MERGE_VERSION 4

struct line_entry
{
	uint32_t line;
	uint32_t address_start;
	uint32_t n_addresses;
};

struct file_data
{
	uint32_t magic;
	uint32_t version;
	uint32_t size;
	uint32_t checksum;
	uint64_t timestamp;
	uint32_t n_entries;
	uint32_t address_table_offset;
	uint32_t file_name_offset;

	struct line_entry entries[];
};

// Unit test stuff
namespace merge_parser
{
class marshal;
class output;
class input;
}

class MergeParser: public IMergeParser
{
public:
	friend class merge_parser::marshal;
	friend class merge_parser::output;
	friend class merge_parser::input;

	MergeParser(IReporter &reporter, const std::string &baseDirectory, const std::string &outputDirectory, IFilter &filter) :
			m_baseDirectory(baseDirectory), m_outputDirectory(outputDirectory), m_filter(filter)
	{
		reporter.registerListener(*this);
	}

	~MergeParser()
	{
		for (FileByNameMap_t::iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *cur = it->second;

			delete cur;
		}

		m_files.clear();
	}

	// From IFileParser
	virtual bool addFile(const std::string &filename, struct phdr_data_entry *phdr_data)
	{
		return true;
	}

	virtual bool setMainFileRelocation(unsigned long relocation)
	{
		return true;
	}

	virtual void registerLineListener(IFileParser::ILineListener &listener)
	{
		m_lineListeners.push_back(&listener);
	}

	// Unused ...
	virtual void registerFileListener(IFileParser::IFileListener &listener)
	{
	}

	virtual bool parse()
	{
		return true;
	}

	virtual uint64_t getChecksum()
	{
		return 0;
	}

	std::string getParserType()
	{
		return "merge";
	}

	enum IFileParser::PossibleHits maxPossibleHits()
	{
		return IFileParser::HITS_SINGLE; // Hits from multiple binaries
	}

	void setupParser(IFilter *filter)
	{
	}

	// ... to here
	virtual unsigned int matchParser(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		return match_none;
	}

	// From IReporter::IListener
	void onAddress(uint64_t addr, unsigned long hits)
	{
		if (m_fileLineByAddress.find(addr) == m_fileLineByAddress.end())
		{
			m_pendingHits[addr] = hits;
			return;
		}

		addr = m_fileLineByAddress[addr];

		File *file = m_filesByAddress[addr];

		if (!file)
			return;

		file->registerHits(addr, hits);

		for (CollectorListenerList_t::const_iterator it = m_collectorListeners.begin(); it != m_collectorListeners.end();
				++it)
			(*it)->onAddressHit(addr, hits);
	}

	// From IReporter::IListener
	virtual void onLineReporter(const std::string &filename, unsigned int lineNr, uint64_t addr)
	{
		if (!m_filter.runFilters(filename))
		{
			return;
		}

		// Nothing to do in that case
		if (!file_exists(filename))
			return;

		File *file;

		file = m_files[filename];
		if (!file)
		{
			file = new File(filename);

			m_files[filename] = file;
		}

		uint64_t addrHash = hashAddress(filename, lineNr, addr) & ~(1ULL << 63);

		m_fileLineByAddress[addr] = addrHash;

		// Mark as a local file
		file->setLocal();
		file->addLine(lineNr, addrHash);

		// Record this for the collector hits
		m_filesByAddress[addrHash] = file;

		for (LineListenerList_t::const_iterator it = m_lineListeners.begin(); it != m_lineListeners.end(); ++it)
			(*it)->onLine(filename, lineNr, addrHash);

		/*
		 * Visit pending addresses for this file/line. onAddress gets a non-
		 * hashed address, so replicate that behavior here.
		 */
		AddrToHitsMap_t::iterator pend = m_pendingHits.find(addr);
		if (pend != m_pendingHits.end())
		{
			onAddress(addr, pend->second);

			m_pendingHits.erase(addr);
		}
	}

	// From IWriter
	void onStartup()
	{
		(void) mkdir(m_baseDirectory.c_str(), 0755);
		(void) mkdir(m_outputDirectory.c_str(), 0755);
		(void) mkdir(fmt("%s/metadata", m_outputDirectory.c_str()).c_str(), 0755);
	}

	void onStop()
	{
		IConfiguration &conf = IConfiguration::getInstance();
		bool inMergeMode = conf.keyAsInt("running-mode") == IConfiguration::MODE_MERGE_ONLY;

		// Parse data from earlier runs
		if (inMergeMode)
			parseStoredDataMerged();
		else
			parseStoredData();

		/* Produce something like
		 *
		 *   /tmp/kcov/calc/metadata/4f332bca
		 *   /tmp/kcov/calc/metadata/cd9932a1
		 *
		 * For all the files we've covered. The output filename comes from a hash of
		 * the input filename.
		 */
		for (FileByNameMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			/*
			 *  Only marshal local files, except when in merged mode - then all files
			 *  are local (or none, depending on how you see it).
			 */
			if (!inMergeMode && !it->second->m_local)
				continue;

			const struct file_data *fd = marshalFile(it->second->m_filename);

			if (!fd)
				continue;

			uint32_t crc = hash_block((const void *) it->second->m_filename.c_str(), it->second->m_filename.size());
			std::string name = fmt("%08x", crc);

			write_file((const void *) fd, be_to_host<uint32_t>(fd->size), "%s/metadata/%s", m_outputDirectory.c_str(),
					name.c_str());

			free((void *) fd);
		}
	}

	void write()
	{
	}

	// From ICollector
	virtual void registerListener(ICollector::IListener &listener)
	{
		m_collectorListeners.push_back(&listener);
	}

	virtual void registerEventTickListener(ICollector::IEventTickListener &listener)
	{
	}

	virtual int run(const std::string &filename)
	{
		// Not used
		panic("Should not call run here");

		return 0;
	}

private:
	uint64_t hashAddress(const std::string &filename, unsigned int lineNr, uint64_t addr)
	{
		// Convert address into a suitable format for the merge parser
		uint64_t addrHash = (uint64_t) hash_block(filename.c_str(), filename.size()) | ((uint64_t) lineNr << 32ULL);

		return addrHash;
	}

	void parseStoredData()
	{
		DIR *dir;
		struct dirent *de;

		dir = opendir(m_baseDirectory.c_str());
		panic_if(!dir, "Can't open directory %s\n", m_baseDirectory.c_str());

		// Unmarshal and parse all metadata
		for (de = readdir(dir); de; de = readdir(dir))
		{
			std::string cur = m_baseDirectory + de->d_name;

			// ... except for the current coveree
			if (cur == m_outputDirectory)
				continue;

			parseDirectory(cur);
		}
		closedir(dir);
	}

	void parseStoredDataMerged()
	{
		IConfiguration &conf = IConfiguration::getInstance();
		const char **argv = conf.getArgv();
		unsigned int argc = conf.getArgc();

		// argv[] contains the directories to merge
		for (unsigned int i = 0; i < argc; i++)
		{
			DIR *dir;
			struct dirent *de;

			dir = opendir(argv[i]);

			if (!dir)
			{
				warning("kcov: Can't open directory %s\n", argv[i]);
				continue;
			}

			// Unmarshal and parse all metadata
			for (de = readdir(dir); de; de = readdir(dir))
			{
				std::string cur = fmt("%s/%s", argv[i], de->d_name);

				parseDirectory(cur);
			}
			closedir(dir);
		}
	}

	void parseDirectory(const std::string &dirName)
	{
		DIR *dir;
		struct dirent *de;
		std::string metadataDirName = dirName + "/metadata";

		dir = opendir(metadataDirName.c_str());
		// Can occur naturally
		if (!dir)
			return;

		// Read all metadata from the directory
		for (de = readdir(dir); de; de = readdir(dir))
			parseOne(metadataDirName, de->d_name);

		closedir(dir);
	}

	void parseOne(const std::string &metadataDirName, const std::string &curFile)
	{
		size_t size;

		// Not as hash?
		if (!string_is_integer(curFile, 16))
			return;

		struct file_data *fd = (struct file_data *)read_file(&size, "%s/%s", metadataDirName.c_str(), curFile.c_str());
		if (!fd)
			return;

		if (size >= sizeof(struct file_data) && unMarshalFile(fd))
		{
			parseFileData(fd);
		}

		free(fd);
	}

	void parseFileData(struct file_data *fd)
	{
		std::string filename((const char *) fd + fd->file_name_offset);

		filename = m_filter.mangleSourcePath(filename);

		// File has been removed since last test
		if (!file_exists(filename))
			return;

		File *file;

		file = m_files[filename];
		if (!file)
		{
			file = new File(filename);

			m_files[filename] = file;
		}
		else
		{
			// Checksum doesn't match, ignore this file
			if (file->m_checksum != fd->checksum)
				return;
		}

		uint64_t *addrTable = (uint64_t *) ((char *) fd + fd->address_table_offset);
		for (unsigned i = 0; i < fd->n_entries; i++)
		{
			uint32_t lineNr = fd->entries[i].line;

			for (unsigned ia = 0; ia < fd->entries[i].n_addresses; ia++)
			{
				uint64_t addr = addrTable[fd->entries[i].address_start + ia];

				// Check if this was a hit (and remove the hit bit from the address)
				bool hit = (addr & (1ULL << 63));
				addr &= ~(1ULL << 63);

				file->addLine(lineNr, addr);

				for (LineListenerList_t::const_iterator it = m_lineListeners.begin(); it != m_lineListeners.end(); ++it)
					(*it)->onLine(filename, lineNr, addr & ~(1ULL << 63));

				// Register and report the hit
				if (hit)
				{
					file->registerHits(addr, 1);

					for (CollectorListenerList_t::const_iterator itC = m_collectorListeners.begin();
							itC != m_collectorListeners.end(); ++itC)
						(*itC)->onAddressHit(addr, 1);
				}
			}
		}
	}

	const struct file_data *marshalFile(const std::string &filename)
	{
		File *file = m_files[filename];

		if (!file)
			return NULL;

		uint32_t n_addrs = 0;
		for (LineAddrMap_t::const_iterator it = file->m_lines.begin(); it != file->m_lines.end(); ++it)
		{
			n_addrs += it->second.size();
		}

		// Header + each line + the filename
		size_t size = sizeof(struct file_data) + file->m_lines.size() * sizeof(struct line_entry)
				+ n_addrs * sizeof(uint64_t) + file->m_filename.size() + 1 + 8; // allow padding of the address table
		struct file_data *out = (struct file_data *) xmalloc(size);

		out->magic = to_be<uint32_t>(MERGE_MAGIC);
		out->version = to_be<uint32_t>(MERGE_VERSION);
		out->checksum = to_be<uint32_t>(file->m_checksum);
		out->timestamp = to_be<uint64_t>(file->m_fileTimestamp);
		out->size = to_be<uint32_t>(size);
		out->n_entries = to_be<uint32_t>(file->m_lines.size());

		struct line_entry *p = out->entries;

		// Point to address table
		uint64_t *addrTable = (uint64_t *) ((char *) p + sizeof(struct line_entry) * file->m_lines.size());
		unsigned long addrTable_8 = (unsigned long) addrTable;
		uint32_t tableOffset = 0;

		// 64-bit align address table
		if ((addrTable_8 & 7) != 0)
		{
			addrTable_8 += 8 - (addrTable_8 & 7);

			addrTable = (uint64_t *) addrTable_8;
		}

		for (LineAddrMap_t::const_iterator it = file->m_lines.begin(); it != file->m_lines.end(); ++it)
		{
			uint32_t line = it->first;
			p->line = to_be<uint32_t>(line);

			p->address_start = to_be<uint32_t>(tableOffset);
			p->n_addresses = to_be<uint32_t>(it->second.size());
			for (AddrMap_t::const_iterator itAddr = it->second.begin(); itAddr != it->second.end(); ++itAddr)
			{
				uint64_t addr = itAddr->first;

				if (file->m_addrHits[addr])
					addr |= (1ULL << 63);

				addrTable[tableOffset] = to_be<uint64_t>(addr);

				tableOffset++;
			}

			p++;
		}

		uint32_t tableStart = (uint32_t) ((char *) addrTable - (char *) out);
		out->address_table_offset = to_be<uint32_t>(tableStart);
		char *p_name = (char *) out + tableStart + tableOffset * sizeof(uint64_t);

		// Allocated with the terminator above
		strcpy(p_name, file->m_filename.c_str());
		out->file_name_offset = to_be<uint32_t>(p_name - (char *) out);

		return out;
	}

	bool unMarshalFile(struct file_data *fd)
	{
		fd->magic = be_to_host<uint32_t>(fd->magic);
		fd->version = be_to_host<uint32_t>(fd->version);
		fd->checksum = be_to_host<uint32_t>(fd->checksum);
		fd->timestamp = be_to_host<uint64_t>(fd->timestamp);
		fd->size = be_to_host<uint32_t>(fd->size);
		fd->n_entries = be_to_host<uint32_t>(fd->n_entries);
		fd->file_name_offset = be_to_host<uint32_t>(fd->file_name_offset);
		fd->address_table_offset = be_to_host<uint32_t>(fd->address_table_offset);

		if (fd->magic != MERGE_MAGIC)
			return false;

		if (fd->version != MERGE_VERSION)
			return false;

		struct line_entry *p = fd->entries;

		// Unmarshal entries...
		for (unsigned i = 0; i < fd->n_entries; i++)
		{
			p->line = be_to_host<uint32_t>(p->line);
			p->n_addresses = be_to_host<uint32_t>(p->n_addresses);
			p->address_start = be_to_host<uint32_t>(p->address_start);

			p++;
		}

		// ... and the address table
		uint64_t *addressTable = (uint64_t *) ((char *) fd + fd->address_table_offset);
		for (unsigned i = 0; i < (fd->file_name_offset - fd->address_table_offset) / sizeof(uint64_t); i++)
			addressTable[i] = be_to_host<uint64_t>(addressTable[i]);

		return true;
	}

	typedef std::unordered_map<uint64_t, unsigned int> AddrMap_t;
	typedef std::unordered_map<unsigned int, AddrMap_t> LineAddrMap_t;

	class File
	{
	public:
		File(const std::string &filename) :
				m_filename(filename), m_local(false)
		{
			void *data;
			size_t size;

			data = read_file(&size, "%s", filename.c_str());
			panic_if(!data, "File %s exists, but can't be read???", filename.c_str());
			m_checksum = hash_block(data, size);
			m_fileTimestamp = get_file_timestamp(filename.c_str());

			free((void *) data);
		}

		void setLocal()
		{
			m_local = true;
		}

		void addLine(unsigned int lineNr, uint64_t addr)
		{
			m_lines[lineNr][addr]++;
		}

		void registerHits(uint64_t addr, unsigned int hits)
		{
			m_addrHits[addr] += hits;
		}

		std::string m_filename;
		uint64_t m_fileTimestamp;
		LineAddrMap_t m_lines;
		AddrMap_t m_addrHits;
		uint32_t m_checksum;
		bool m_local;
	};

	typedef std::vector<ICollector::IListener *> CollectorListenerList_t;
	typedef std::unordered_map<std::string, File *> FileByNameMap_t;
	typedef std::unordered_map<uint64_t, File *> FileByAddressMap_t;
	typedef std::unordered_map<uint64_t, uint64_t> FileLineByAddress_t;
	typedef std::unordered_map<uint64_t, unsigned long> AddrToHitsMap_t;
	typedef std::unordered_map<uint64_t, unsigned long> AddressByFileLine_t;
	typedef std::vector<IFileParser::ILineListener *> LineListenerList_t;

	// All files in the current coverage session
	FileByNameMap_t m_files;
	FileByAddressMap_t m_filesByAddress;
	FileLineByAddress_t m_fileLineByAddress;
	AddrToHitsMap_t m_pendingHits;

	LineListenerList_t m_lineListeners;
	const std::string m_baseDirectory;
	const std::string m_outputDirectory;

	CollectorListenerList_t m_collectorListeners;
	IFilter &m_filter;
};

namespace kcov
{
IMergeParser &createMergeParser(IReporter &reporter, const std::string &baseDirectory, const std::string &outputDirectory,
		IFilter &filter)
{
	return *new MergeParser(reporter, baseDirectory, outputDirectory, filter);
}
}
