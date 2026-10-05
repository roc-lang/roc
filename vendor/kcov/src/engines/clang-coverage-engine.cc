#include <engine.hh>
#include <utils.hh>
#include <configuration.hh>
#include <capabilities.hh>
#include <file-parser.hh>
#include <disassembler.hh>
#include <elf.hh>

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <signal.h>

#include <vector>
#include <unordered_map>
#include <utility> // std::pair

#include "../parsers/dwarf.hh"
#include "script-engine-base.hh"

using namespace kcov;

class ClangEngine : public ScriptEngineBase, IFileParser::ILineListener
{
public:
	ClangEngine() :
			ScriptEngineBase(), m_child(-1), m_checksum(0)
	{
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		char * const *argv = (char * const *) conf.getArgv();

		m_listener = &listener;

		// Run the program until completion
		m_child = fork();
		if (m_child == 0)
		{
			std::string env = fmt("ASAN_OPTIONS=coverage=1:coverage_dir=%s", conf.keyAsString("target-directory").c_str());

			char *cpy = xstrdup(env.c_str());

			putenv(cpy);
			unsetenv("LD_PRELOAD");
			execv(argv[0], argv);
		}
		else if (m_child < 0)
		{
			perror("fork");

			return false;
		}

		return true;
	}

	bool continueExecution()
	{
		if (m_child < 0)
			return false;

		int status = 0;

		// Wait for the child
		waitpid(m_child, &status, 0);

		m_child = -1;

		// All coverage collection is done after the program has been run
		DIR *dir;
		struct dirent *de;

		std::string targetDir = IConfiguration::getInstance().keyAsString("target-directory");
		dir = opendir(targetDir.c_str());
		panic_if(!dir, "Can't open directory\n");

		for (de = readdir(dir); de; de = readdir(dir))
		{
			std::string cur = fmt("%s/%s", targetDir.c_str(), de->d_name);

			// ... except for the current coveree
			if (cur.find(".sancov") == std::string::npos)
				continue;

			parseCoverageFile(cur);
		}
		closedir(dir);

		Event ev(ev_exit, WEXITSTATUS(status));

		// Report the exit status
		m_listener->onEvent(ev);

		return false;
	}

	std::string getParserType()
	{
		return "clang-sanitizer";
	}

	virtual uint64_t getChecksum()
	{
		return m_checksum;
	}

	unsigned int matchParser(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		if (IConfiguration::getInstance().keyAsInt("clang-sanitizer"))
			return match_perfect;

		return match_none;
	}

	virtual enum IFileParser::PossibleHits maxPossibleHits()
	{
		return IFileParser::HITS_SINGLE;
	}

	void kill(int signal)
	{
	}

	bool addFile(const std::string &filename, struct phdr_data_entry *phdr_data)
	{
		IElf *elf = IElf::create(filename);

		if (!elf)
			return false;

		const std::vector<Segment> &segs = elf->getSegments();
		for (std::vector<Segment>::const_iterator it = segs.begin(); it != segs.end(); ++it)
			IDisassembler::getInstance().addSection(it->getData(), it->getSize(), it->getBase());

		bool rv = m_dwarfParser.open(filename);

		// Get a list of all possible source lines
		if (rv)
			m_dwarfParser.forEachLine(*this);

		size_t sz;
		uint8_t *p = (uint8_t *) read_file(&sz, "%s", filename.c_str());

		m_checksum = hash_block(p, sz);

		free(p);

		return rv;
	}

private:
	typedef std::vector<std::string> FileList_t;

	void onLine(const std::string &file, unsigned int lineNr, uint64_t addr)
	{

		for (LineListenerList_t::const_iterator it = m_lineListeners.begin(); it != m_lineListeners.end(); ++it)
			(*it)->onLine(get_real_path(file), lineNr, addr);
	}

	void parseCoverageFile(const std::string &name)
	{
		size_t sz;
		void *p;

		p = read_file(&sz, "%s", name.c_str());
		if (!p)
		{
			return;
		}

		// Remove the coverage file for the next round
		unlink(name.c_str());

		// Only header?
		if (sz < 8)
		{
			free(p);
			return;
		}

		uint64_t header = *(uint64_t *) p;
		// Assume native-endianness (?)
		if (header == 0xC0BFFFFFFFFFFF64ULL)
		{
			size_t nEntries = (sz - sizeof(uint64_t)) / sizeof(uint64_t);
			uint64_t *entries = &((uint64_t *) p)[1];

			for (size_t i = 0; i < nEntries; i++)
			{
				m_dwarfParser.forAddress(*this, entries[i] + 1);
				reportBreakpoint(entries[i] + 1);
			}
		}
		else if (header == 0xC0BFFFFFFFFFFF32ULL)
		{
			size_t nEntries = (sz - sizeof(uint64_t)) / sizeof(uint32_t);
			uint32_t *entries = &((uint32_t *) p)[1];

			for (size_t i = 0; i < nEntries; i++)
			{
				m_dwarfParser.forAddress(*this, entries[i] + 1);
				reportBreakpoint(entries[i] + 1);
			}
		}

		free(p);
	}

	void reportBreakpoint(uint64_t address)
	{
		std::vector<uint64_t> bb = IDisassembler::getInstance().getBasicBlock(address);

		for (std::vector<uint64_t>::iterator it = bb.begin(); it != bb.end(); ++it)
			reportEvent(ev_breakpoint, 0, *it);

		// Fallback in case kcov is broken
		if (bb.empty())
		{
			kcov_debug(ENGINE_MSG, "Address 0x%llx not in a basic block\n", (long long) address);
			reportEvent(ev_breakpoint, 0, address);
		}
	}

	pid_t m_child;
	DwarfParser m_dwarfParser;
	uint64_t m_checksum;
};

static ClangEngine *g_clangEngine;
class ClangCtor
{
public:
	ClangCtor()
	{
		g_clangEngine = new ClangEngine();
	}
};
static ClangCtor g_clangCtor;

class ClangEngineCreator: public IEngineFactory::IEngineCreator
{
public:
	virtual ~ClangEngineCreator()
	{
	}

	virtual IEngine *create()
	{
		return g_clangEngine;
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		if (IConfiguration::getInstance().keyAsInt("clang-sanitizer"))
			return match_perfect;

		return match_none;
	}
};

static ClangEngineCreator g_clangEngineCreator;
