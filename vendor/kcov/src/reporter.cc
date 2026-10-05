#include <reporter.hh>
#include <file-parser.hh>
#include <collector.hh>
#include <utils.hh>
#include <filter.hh>
#include <configuration.hh>
#include <source-file-cache.hh>

#include <string>
#include <list>
#include <unordered_map>
#include <functional>
#include <map>
#include <fstream>

#include "swap-endian.hh"

using namespace kcov;

#define KCOV_MAGIC      0x6b636f76 /* "kcov" */
#define KCOV_DB_VERSION 6

struct marshalHeaderStruct
{
	uint32_t magic;
	uint32_t db_version;
	uint64_t checksum;
};

class Reporter : public IReporter,
		public IFileParser::ILineListener,
		public IFileParser::IFileListener,
		public ICollector::IListener,
		public IReporter::IListener
{
public:
	Reporter(IFileParser &fileParser, ICollector &collector, IFilter &filter) :
			m_fileParser(fileParser), m_collector(collector), m_filter(filter), m_maxPossibleHits(
					fileParser.maxPossibleHits()), m_unmarshallingDone(false), m_order(1) // "First" hit - 0 marks unset
	{
		m_fileParser.registerLineListener(*this);
		m_fileParser.registerFileListener(*this);
		m_collector.registerListener(*this);

		m_hashFilename = fileParser.getParserType() == "ELF";

		m_dbFileName = IConfiguration::getInstance().keyAsString("target-directory") + "/coverage.db";
	}

	~Reporter()
	{
		if (IConfiguration::getInstance().keyAsInt("running-mode") !=
            IConfiguration::MODE_REPORT_ONLY)
        {
			writeCoverageDatabase();
		}

		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *cur = it->second;

			delete cur;
		}
	}

	void registerListener(IReporter::IListener &listener)
	{
		m_listeners.push_back(&listener);
	}

	bool fileIsIncluded(const std::string &file)
	{
		return m_filter.runFilters(file);
	}

	bool lineIsCode(const std::string &file, unsigned int lineNr)
	{
		FileMap_t::iterator it = m_files.find(file);

		// Not code if the file doesn't exist!
		if (it == m_files.end())
			return false;

		return it->second->lineIsCode(lineNr);
	}

	LineExecutionCount getLineExecutionCount(const std::string &file, unsigned int lineNr)
	{
		unsigned int hits = 0;
		unsigned int possibleHits = 0;
		uint64_t order = 0;

		FileMap_t::const_iterator it = m_files.find(file);

		if (it != m_files.end())
		{
			const Line *line = it->second->getLine(lineNr);

			if (line && !line->isUnreachable())
			{
				hits = line->hits();
				possibleHits = line->possibleHits(m_maxPossibleHits != IFileParser::HITS_UNLIMITED);
				order = line->getOrder();
			}
		}

		return LineExecutionCount(hits, possibleHits, order);
	}

	ExecutionSummary getExecutionSummary()
	{
		unsigned int executedLines = 0;
		unsigned int nrLines = 0;

		// Summarize all files
		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			const std::string &fileName = it->first;
			const File *file = it->second;

			// Don't include non-existing files in summary
			if (!file_exists(fileName))
				continue;

			// And not filtered ones either
			if (!m_filter.runFilters(fileName))
				continue;

			executedLines += file->getExecutedLines();
			nrLines += file->getNrLines();
		}

		return ExecutionSummary(nrLines, executedLines);
	}

	void *marshal(size_t *szOut)
	{
		size_t sz = getMarshalSize();
		void *start;
		uint8_t *p;

		start = malloc(sz);
		if (!start)
			return NULL;
		memset(start, 0, sz);
		p = marshalHeader((uint8_t *) start);

		// Marshal all lines in the files
		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			const File *cur = it->second;

			p = cur->marshal(p, *this);
		}

		*szOut = sz;

		return start;
	}

	bool unMarshal(void *data, size_t sz)
	{
		uint8_t *start = (uint8_t *) data;
		uint8_t *p = start;
		size_t n;

		p = unMarshalHeader(p);

		if (!p)
			return false;

		n = (sz - (p - start)) / getMarshalEntrySize();

		for (size_t i = 0; i < n; i++)
		{
			uint64_t fileHash;
			uint64_t addr;
			uint64_t hits;
			uint64_t addrIndex;

			p = Line::unMarshal(p, &addr, &hits, &fileHash, &addrIndex);

			if (!hits)
				continue;

			AddrToLineMap_t::iterator it = m_addrToLine.find(addr);

			/*
			 * Can't find this file/line
			 *
			 * Either it doesn't exist in the binary, or it hasn't been parsed
			 * yet, which will be the case for bash/python. Add it to pending
			 * addresses if so.

			 * Typically because it's in a shared library, which hasn't been
			 * loaded yet.
			 */
			if (it == m_addrToLine.end())
			{
				LineIdToFileMap_t::iterator lit = m_lineIdToFileMap.find(fileHash);

				if (lit == m_lineIdToFileMap.end())
				{
					// No line ID (shared library?). Add to pending
					m_pendingFiles[fileHash].push_back(PendingFileAddress(addrIndex, hits));
				}
				else
				{
					// line ID exists, but not address (PIEs etc)
					reportAddress(fileHash, hits);

					lit->second->registerHitIndex(addrIndex, hits, m_maxPossibleHits != IFileParser::HITS_UNLIMITED);
				}

				continue;
			}

			// Part of this file - just report it
			onAddressHit(addr, hits);
		}

		return true;
	}

	virtual void writeCoverageDatabase()
	{
		size_t sz;
		void *data = marshal(&sz);

		if (data)
			write_file(data, sz, "%s", m_dbFileName.c_str());

		free(data);
	}

private:
	size_t getMarshalEntrySize()
	{
		return 4 * sizeof(uint64_t);
	}

	size_t getMarshalSize()
	{
		size_t out = 0;

		// Sum all file sizes
		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			out += it->second->marshalSize();
		}

		return out * getMarshalEntrySize() + sizeof(struct marshalHeaderStruct);
	}

	uint8_t *marshalHeader(uint8_t *p)
	{
		struct marshalHeaderStruct *hdr = (struct marshalHeaderStruct *)p;

		hdr->magic = to_be<uint32_t>(KCOV_MAGIC);
		hdr->db_version = to_be<uint32_t>(KCOV_DB_VERSION);
		hdr->checksum = to_be<uint64_t>(m_fileParser.getChecksum());

		return p + sizeof(struct marshalHeaderStruct);
	}

	uint8_t *unMarshalHeader(uint8_t *p)
	{
		struct marshalHeaderStruct *hdr = (struct marshalHeaderStruct *)p;

		if (be_to_host<uint32_t>(hdr->magic) != KCOV_MAGIC)
			return NULL;

		if (be_to_host<uint32_t>(hdr->db_version) != KCOV_DB_VERSION)
			return NULL;

		if (be_to_host<uint64_t>(hdr->checksum) != m_fileParser.getChecksum())
			return NULL;

		return p + sizeof(struct marshalHeaderStruct);
	}

	/* Called when the file is parsed */
	void onLine(const std::string &file, unsigned int lineNr, uint64_t addr)
	{
		if (!m_filter.runFilters(file))
			return;

		kcov_debug(INFO_MSG, "REPORT %s:%u at 0x%lx\n", file.c_str(), lineNr, (unsigned long) addr);

		File *fp = m_files[file];

		if (!fp)
		{
			uint64_t hash = 0;

			/*
			 * Hash filenames for ELF parsers, the contents otherwise.
			 *
			 * Binaries are protected by an ELF checksum, but e.g., bash scripts need
			 * to be identified by the contents.
			 */
			if (!m_hashFilename)
			{
				size_t sz;
				void *data = read_file(&sz, "%s", file.c_str());

				// Compute checksum by contents
				if (data)
					hash = hash_block(data, sz);

				free(data);
			}
			else
			{
				hash = m_fileHash(file);
			}

			fp = new File(hash);

			// Mark unreachable lines separately (often none)
			const std::vector<std::string> &lines = ISourceFileCache::getInstance().getLines(file);
			for (unsigned int nr = 1; nr <= lines.size(); nr++)
			{
				if (!m_filter.runLineFilters(file, lineNr, lines[nr - 1]))
				{
					Line *line = new Line(fp->getFileHash(), nr, true);

					fp->addLine(nr, line);
				}
			}

			m_files[file] = fp;
		}

		Line *line = fp->getLine(lineNr);

		if (!line)
		{

			line = new Line(fp->getFileHash(), lineNr);
			fp->addLine(lineNr, line);
		}

		uint64_t lineId = line->lineId();

		line->addAddress(addr);
		bool dup = false;
		for (LineList_t::iterator it = m_addrToLine[addr].begin();
			it != m_addrToLine[addr].end();
			++it)
		{
			if (line->lineId() == (*it)->lineId())
			{
				dup = true;
			}
		}
		if (!dup)
		{
			m_addrToLine[addr].push_back(line);
		}
		m_lineIdToFileMap[lineId] = line;

		// Report pending addresses for this file/line
		PendingFilesMap_t::const_iterator it = m_pendingFiles.find(lineId);
		if (it != m_pendingFiles.end())
		{
			for (PendingHitsList_t::const_iterator fit = it->second.begin(); fit != it->second.end(); ++fit)
			{
				const PendingFileAddress &val = *fit;
				unsigned long hits = val.m_hits;
				uint64_t index = val.m_index;

				reportAddress(lineId, hits);

				line->registerHitIndex(index, hits, m_maxPossibleHits != IFileParser::HITS_UNLIMITED);
			}

			// Handled now
			m_pendingFiles[lineId].clear();
		}

		for (ListenerList_t::const_iterator it = m_listeners.begin(); it != m_listeners.end(); ++it)
			(*it)->onLineReporter(file, lineNr, lineId);
	}

	// Called when a file is added (e.g., a shared library)
	void onFile(const IFileParser::File &file)
	{
		// Only unmarshal once
		if (m_unmarshallingDone)
			return;

		if (!IConfiguration::getInstance().keyAsInt("clean-output"))
		{
			void *data;
			size_t sz;

			data = read_file(&sz, "%s", m_dbFileName.c_str());

			if (data && !unMarshal(data, sz))
				kcov_debug(INFO_MSG, "Can't unmarshal %s\n", m_dbFileName.c_str());

			free(data);
		}
		m_unmarshallingDone = true;
	}

	/* Called during runtime */
	void reportAddress(uint64_t lineHash, unsigned long hits)
	{
		// Report the line hash (losing partial hit info from now on, but
		// that's only for the merge-reporter anyway)
		for (ListenerList_t::const_iterator it = m_listeners.begin(); it != m_listeners.end(); ++it)
			(*it)->onAddress(lineHash, hits);
	}

	// From ICollector::IListener
	void onAddressHit(uint64_t addr, unsigned long hits)
	{
		AddrToLineMap_t::iterator it = m_addrToLine.find(addr);

//		kcov_debug(INFO_MSG, "%p (bef) REPORT hit at 0x%llx, %d lines\n", this, (unsigned long long)addr, m_addrToLine.size());

		if (it == m_addrToLine.end())
			return;

		kcov_debug(INFO_MSG, "%p REPORT hit at 0x%llx\n", this, (unsigned long long)addr);

		for (LineList_t::iterator lit = it->second.begin();
                            lit != it->second.end();
                            ++lit) {
                        Line* line = *lit;

			line->registerHit(addr, hits, m_maxPossibleHits != IFileParser::HITS_UNLIMITED);

			// Setup the hit order
			if (line->getOrder() == 0) {
				line->setOrder(m_order);
				m_order++;
			}

			reportAddress(line->lineId(), hits);
		}
	}

	// From IReporter::IListener - report recursively
	void onAddress(uint64_t addr, unsigned long hits)
	{
	}

	class Line
	{
	public:
		// More efficient than an unordered_map
		typedef std::vector<std::pair<uint64_t, int>> AddrToHitsMap_t;

		Line(uint64_t fileHash, unsigned int lineNr, bool unreachable = false) :
				m_lineId((fileHash << 32ULL) | lineNr), m_order(0), m_unreachable(unreachable)
		{
		}

		bool isUnreachable() const
		{
			return m_unreachable;
		}

		uint64_t getOrder() const
		{
			return m_order;
		}

		void setOrder(uint64_t order)
		{
			m_order = order;
		}

		void addAddress(uint64_t addr)
		{
			// Check if it already exists
			for (AddrToHitsMap_t::iterator it = m_addrs.begin(); it != m_addrs.end(); ++it)
			{
				if (it->first == addr)
					return;
			}

			m_addrs.push_back(std::pair<uint64_t, int>(addr, 0));
		}

		void registerHit(uint64_t addr, unsigned long hits, bool singleShot)
		{
			AddrToHitsMap_t::iterator it;

			for (it = m_addrs.begin(); it != m_addrs.end(); ++it)
			{
				if (it->first == addr)
					break;
			}

			// Add one last
			if (it == m_addrs.end())
			{
				m_addrs.push_back(std::pair<uint64_t, int>(addr, 0));
				--it;
			}

			if (singleShot)
				it->second = 1;
			else
				it->second += hits;
		}

		void registerHitIndex(uint64_t index, unsigned long hits, bool singleShot)
		{
			// Avoid broken data
			if (m_addrs.size() <= index)
				return;

			m_addrs[index].second += hits;
		}

		void clearHits()
		{
			for (AddrToHitsMap_t::iterator it = m_addrs.begin(); it != m_addrs.end(); ++it)
				it->second = 0;
		}

		unsigned int hits() const
		{
			unsigned int out = 0;

			for (AddrToHitsMap_t::const_iterator it = m_addrs.begin(); it != m_addrs.end(); ++it)
				out += it->second;

			return out;
		}

		unsigned int possibleHits(bool singleShot) const
		{
			if (singleShot)
				return m_addrs.size();

			return 0; // Meaning any number of hits are possible
		}

		uint64_t lineId() const
		{
			return m_lineId;
		}

		uint8_t *marshal(uint8_t *start, const Reporter &parent)
		{
			uint64_t *data = (uint64_t *) start;
			unsigned int addrIndex = 0;

			for (AddrToHitsMap_t::const_iterator it = m_addrs.begin(); it != m_addrs.end(); ++it, addrIndex++)
			{
				// No hits? Ignore if so
				if (!it->second)
					continue;

				uint64_t hash = m_lineId;
				uint64_t addr = it->first;

				// Address, hash, index and number of hits
				*data++ = to_be<uint64_t>(addr);
				*data++ = to_be<uint64_t>(hash);
				*data++ = to_be<uint64_t>(addrIndex);
				*data++ = to_be<uint64_t>(it->second);
			}

			return (uint8_t *) data;
		}

		size_t marshalSize() const
		{
			unsigned int n = 0;

			for (AddrToHitsMap_t::const_iterator it = m_addrs.begin(); it != m_addrs.end(); ++it)
			{
				// No hits? Ignore if so
				if (!it->second)
					continue;

				n++;
			}

			// Address, file hash and number of hits (64-bit values)
			return n * sizeof(uint64_t) * 4;
		}

		static uint8_t *unMarshal(uint8_t *p, uint64_t *outAddr, uint64_t *outHits, uint64_t *outFileHash,
				uint64_t *outIndex)
		{
			uint64_t *data = (uint64_t *) p;

			*outAddr = be_to_host<uint64_t>(*data++);
			*outFileHash = be_to_host<uint64_t>(*data++);
			*outIndex = be_to_host<uint64_t>(*data++);
			*outHits = be_to_host<uint64_t>(*data++);

			return (uint8_t *) data;
		}

	private:
		AddrToHitsMap_t m_addrs;
		uint64_t m_lineId;
		uint64_t m_order;
		bool m_unreachable;
	};

	class File
	{
	public:
		File(uint64_t hash) :
				m_fileHash(hash), m_nrLines(0)
		{
		}

		~File()
		{
			for (unsigned int i = 0; i < m_lines.size(); i++)
			{
				Line *cur = m_lines[i];

				if (!cur)
					continue;

				delete cur;
			}
		}

		void addLine(unsigned int lineNr, Line *line)
		{
			// Resize the vector to fit this line
			if (lineNr >= m_lines.size())
				m_lines.resize(lineNr + 1);

			m_lines[lineNr] = line;
			if (!line->isUnreachable())
				m_nrLines++;
		}

		Line *getLine(unsigned int lineNr) const
		{
			if (lineNr >= m_lines.size())
				return NULL;

			return m_lines[lineNr];
		}

		uint64_t getFileHash() const
		{
			return m_fileHash;
		}

		// Marshal all line data
		uint8_t *marshal(uint8_t *p, const Reporter &reporter) const
		{
			for (unsigned int i = 0; i < m_lines.size(); i++)
			{
				Line *cur = m_lines[i];

				if (!cur)
					continue;

				p = cur->marshal(p, reporter);
			}

			return p;
		}

		size_t marshalSize() const
		{
			size_t out = 0;

			for (unsigned int i = 0; i < m_lines.size(); i++)
			{
				Line *cur = m_lines[i];

				if (!cur)
					continue;

				out += cur->marshalSize();
			}

			return out;
		}

		unsigned int getExecutedLines() const
		{
			unsigned int out = 0;

			for (unsigned int i = 0; i < m_lines.size(); i++)
			{
				Line *cur = m_lines[i];

				if (!cur)
					continue;

				if (cur->isUnreachable())
					continue;

				// Hits as zero or one (executed or not)
				out += !!cur->hits();
			}

			return out;
		}

		unsigned int getNrLines() const
		{
			return m_nrLines;
		}

		bool lineIsCode(unsigned int lineNr) const
		{
			if (lineNr >= m_lines.size())
				return false;

			if (!m_lines[lineNr])
				return false;

			return !m_lines[lineNr]->isUnreachable();
		}

	private:
		uint64_t m_fileHash;
		std::vector<Line *> m_lines;
		unsigned int m_nrLines;
	};

	class PendingFileAddress
	{
	public:
		PendingFileAddress(uint64_t index, unsigned long hits) :
				m_index(index), m_hits(hits)
		{
		}

		uint64_t m_index;
		unsigned long m_hits;
	};

	typedef std::vector<Line *> LineList_t;
	typedef std::unordered_map<std::string, File *> FileMap_t;
	typedef std::unordered_map<uint64_t, LineList_t> AddrToLineMap_t;
	typedef std::unordered_map<uint64_t, unsigned long> AddrToHitsMap_t;
	typedef std::vector<IReporter::IListener *> ListenerList_t;
	typedef std::unordered_map<uint64_t, Line *> LineIdToFileMap_t;
	typedef std::vector<PendingFileAddress> PendingHitsList_t; // Address, hits
	typedef std::unordered_map<uint64_t, PendingHitsList_t> PendingFilesMap_t;

	FileMap_t m_files;
	AddrToLineMap_t m_addrToLine;
	AddrToHitsMap_t m_pendingHits;
	ListenerList_t m_listeners;
	PendingFilesMap_t m_pendingFiles;
	LineIdToFileMap_t m_lineIdToFileMap;
	std::hash<std::string> m_fileHash;
	bool m_hashFilename;

	IFileParser &m_fileParser;
	ICollector &m_collector;
	IFilter &m_filter;
	enum IFileParser::PossibleHits m_maxPossibleHits;

	bool m_unmarshallingDone;
	std::string m_dbFileName;

	uint64_t m_order;
};

// The merge mode doesn't have/need a proper reporter
class DummyReporter : public IReporter
{
	virtual void registerListener(IListener &listener)
	{
	}

	virtual bool fileIsIncluded(const std::string &file)
	{
		return false;
	}

	virtual bool lineIsCode(const std::string &file, unsigned int lineNr)
	{
		return false;
	}

	virtual LineExecutionCount getLineExecutionCount(const std::string &file, unsigned int lineNr)
	{
		return LineExecutionCount(0, 0, 0);
	}
	virtual ExecutionSummary getExecutionSummary()
	{
		return ExecutionSummary();
	}

	void writeCoverageDatabase()
	{
	}
};

IReporter &IReporter::create(IFileParser &parser, ICollector &collector, IFilter &filter)
{
	return *new Reporter(parser, collector, filter);
}

IReporter &IReporter::createDummyReporter()
{
	return *new DummyReporter();
}
