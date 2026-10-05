#include <file-parser.hh>
#include <engine.hh>
#include <configuration.hh>
#include <output-handler.hh>
#include <utils.hh>
#include <generated-data-base.hh>

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <signal.h>

#include <list>
#include <unordered_map>
#include <vector>
#include <utility>

#include <system-mode/file-data.hh>

#include "system-mode-file-format.hh"

using namespace kcov;

class SystemModeEngine : public IEngine
{
public:
	enum mode
	{
		mode_unset,
		mode_write_file,
		mode_read_file,
	};


	SystemModeEngine(enum mode mode) :
		m_mode(mode),
		m_listener(NULL),
		m_breakpointIdx(0)
	{
	}

	int registerBreakpoint(unsigned long addr)
	{
		m_indexToAddress.push_back(addr);
		m_breakpointIdx++;

		return m_breakpointIdx - 1;
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		m_filename = executable;
		m_listener = &listener;

		return true;
	}

	void kill(int sig)
	{
		// Not applicable
	}

	bool continueExecution()
	{
		size_t sz;
		void *p = read_file(&sz, "%s", m_filename.c_str());

		if (!p)
		{
			error("Can't read file %s\n", m_filename.c_str());
			return false;
		}
		uint32_t checksum = hash_block(p, sz);

		if (m_mode == mode_write_file)
		{
			handleWriter(p, sz, checksum);
		}
		else if (m_mode == mode_read_file)
		{
			handleReadFile();
		}
		else
		{
			panic("Unknown mode??? Check implementation\n");
		}
		free(p);

		// Nothing to do after this
		reportEvent(ev_exit, 0, 0);

		return false;
	}

private:
	void handleWriter(void *p, size_t sz, uint32_t checksum)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		std::string options = getKcovOptionsString();
		struct stat st;

		if (stat(m_filename.c_str(), &st) < 0)
		{
			error("Can't read input file %s\n", m_filename.c_str());
			return;
		}

		SystemModeFile *sysFile = SystemModeFile::fromRawFile(checksum, m_filename, options.c_str(), p, sz);
		if (!sysFile)
		{
			error("Can't create sys file\n");
			return;
		}

		uint32_t idx = 0;
		for (std::vector<uint64_t>::iterator it = m_indexToAddress.begin();
				it != m_indexToAddress.end();
				++it, ++idx)
		{
			sysFile->addEntry(idx, *it);
		}

		std::string dst = conf.keyAsString("system-mode-write-file");

		size_t processedSize;
		const void *data = sysFile->getProcessedData(processedSize);

		if (data)
		{
			write_file(data, processedSize, "%s", dst.c_str());
			if (chmod(dst.c_str(), st.st_mode) < 0)
			{
				error("Can't chmod???\n");
			}
		}

		free((void *)data);

		std::string patchelf = conf.keyAsString("patchelf-command");
		std::string lib = "libkcov_system.so";
		std::string cmd = fmt("%s --add-needed %s %s", patchelf.c_str(), lib.c_str(), dst.c_str());

		// FIXME! Check if patchelf exists

		int rv = system(cmd.c_str());
		if (WEXITSTATUS(rv) != 0)
		{
			error("patchelf command error %d\n", rv);
		}

		delete sysFile;
	}

	void handleReadFile()
	{
		std::string src = IConfiguration::getInstance().keyAsString("system-mode-read-results-file");

		kcov_system_mode::system_mode_memory *results = kcov_system_mode::diskToMemory(src);

		if (!results)
		{
			return;
		}

		for (unsigned i = 0; i < results->n_entries * 32; i++) // 32 bits per entry
		{
			if (results->indexIsHit(i))
			{
				reportEvent(ev_breakpoint, 0, m_indexToAddress[i]);
			}
		}

		delete results;
	}

	std::string getKcovOptionsString()
	{
		IConfiguration &conf = IConfiguration::getInstance();

		return fmt("%s %s %s %s %s %s ",
				keyListAsString(conf.keyAsList("include-pattern")).c_str(),
				keyListAsString(conf.keyAsList("exclude-pattern")).c_str(),
				keyListAsString(conf.keyAsList("include-path")).c_str(),
				keyListAsString(conf.keyAsList("exclude-path")).c_str(),
				conf.keyAsString("exclude-line").c_str(),
				conf.keyAsString("exclude-region").c_str());
	}

	std::string keyListAsString(const std::vector<std::string> &list)
	{
		std::string out;

		if (list.empty())
		{
			return "";
		}

		for (std::vector<std::string>::const_iterator it = list.begin();
				it != list.end();
				++it)
		{
			out = out + *it + ",";
		}
		out = out.substr(0, out.size() - 1); // Remove last ,

		return out;
	}

	void reportEvent(enum event_type type, int data = -1, uint64_t address = 0)
	{
		if (!m_listener)
			return;

		m_listener->onEvent(Event(type, data, address));
	}

	const enum mode m_mode;

	IEngine::IEventListener *m_listener;
	uint32_t m_breakpointIdx;

	std::vector<uint64_t> m_indexToAddress;
	std::string m_filename;
};


// This ugly stuff was again inherited from bashEngine
class SystemModeEngineCreator : public IEngineFactory::IEngineCreator
{
public:
	virtual ~SystemModeEngineCreator()
	{
	}

	virtual IEngine *create()
	{
		SystemModeEngine::mode mode = SystemModeEngine::mode_read_file;

		if (IConfiguration::getInstance().keyAsString("system-mode-write-file") != "")
		{
			mode = SystemModeEngine::mode_write_file;
		}

		return new SystemModeEngine(mode);
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		IConfiguration &conf = IConfiguration::getInstance();

		if (conf.keyAsString("system-mode-write-file") != "" ||
				conf.keyAsString("system-mode-read-results-file") != "")
		{
			return match_perfect;
		}

		return match_none;
	}

private:
	std::string m_filename;
};

static SystemModeEngineCreator g_systemModeEngineCreator;
