#include <file-parser.hh>
#include <engine.hh>
#include <configuration.hh>
#include <output-handler.hh>
#include <lineid.hh>
#include <utils.hh>

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>

#include <list>
#include <unordered_map>
#include <vector>

#include <swap-endian.hh>

using namespace kcov;

/**
 * Base-class for script-based coverage engines.
 */
class ScriptEngineBase : public IEngine, public IFileParser
{
public:
	ScriptEngineBase() :
			m_listener(NULL)
	{
		IParserManager::getInstance().registerParser(*this);
	}

	virtual ~ScriptEngineBase()
	{
	}

	// From IEngine
	virtual int registerBreakpoint(unsigned long addr)
	{
		// No breakpoints
		return 0;
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

	virtual void registerLineListener(ILineListener &listener)
	{
		m_lineListeners.push_back(&listener);
	}

	virtual void registerFileListener(IFileListener &listener)
	{
		m_fileListeners.push_back(&listener);
	}

	virtual bool parse()
	{
		return true;
	}

	virtual uint64_t getChecksum()
	{
		return 0;
	}

	virtual enum IFileParser::PossibleHits maxPossibleHits()
	{
		return IFileParser::HITS_UNLIMITED;
	}

	virtual void setupParser(IFilter *filter)
	{
	}

protected:
	void reportEvent(enum event_type type, int data = -1, uint64_t address = 0)
	{
		if (!m_listener)
			return;

		m_listener->onEvent(Event(type, data, address));
	}

	void fileLineFound(uint32_t crc, const std::string &filename, unsigned int lineNo)
	{
		uint64_t id = getLineId(filename, lineNo);
		uint64_t address = (uint64_t) crc | ((uint64_t) lineNo << 32ULL);

		if (m_lineIdToAddress.find(id) != m_lineIdToAddress.end())
		{
			// Already reported
			return;
		}

		m_lineIdToAddress[id] = address;

		for (LineListenerList_t::const_iterator lit = m_lineListeners.begin(); lit != m_lineListeners.end(); ++lit)
			(*lit)->onLine(get_real_path(filename).c_str(), lineNo, address);
	}

	typedef std::vector<ILineListener *> LineListenerList_t;
	typedef std::vector<IFileListener *> FileListenerList_t;
	typedef std::unordered_map<std::string, bool> ReportedFileMap_t;
	typedef std::unordered_map<uint64_t, uint64_t> LineIdToAddressMap_t;

	LineListenerList_t m_lineListeners;
	FileListenerList_t m_fileListeners;
	ReportedFileMap_t m_reportedFiles;
	LineIdToAddressMap_t m_lineIdToAddress;

	IEventListener *m_listener;
};
