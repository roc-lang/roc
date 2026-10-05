#include <engine.hh>
#include <utils.hh>
#include <configuration.hh>
#include <file-parser.hh>
#include <gcov.hh>

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>

#include <vector>
#include <unordered_map>
#include <utility> // std::pair

using namespace kcov;

class GcovEngine : public IEngine, IFileParser::IFileListener
{
public:
	GcovEngine(IFileParser &parser) :
			m_listener(NULL), m_child(-1)
	{
		parser.registerFileListener(*this);
	}

	int registerBreakpoint(unsigned long addr)
	{
		return 0;
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		char * const *argv = (char * const *) IConfiguration::getInstance().getArgv();

		m_listener = &listener;

		// Run the program until completion
		m_child = fork();
		if (m_child == 0)
		{
			execv(argv[0], argv);
		}
		else if (m_child < 0)
		{
			perror("fork");

			return false;
		}

		return true;
	}

	void kill(int sig)
	{
	}

	bool continueExecution()
	{
		if (m_child < 0)
			return false;

		int status;

		// Wait for the child
		waitpid(m_child, &status, 0);

		m_child = -1;

		// All coverage collection is done after the program has been run
		for (FileList_t::const_iterator it = m_gcdaFiles.begin(); it != m_gcdaFiles.end(); ++it)
		{
			const std::string &gcda = *it;
			std::string gcno = gcda;

			size_t sz = gcno.size();

			// .gcda -> .gcno
			gcno[sz - 2] = 'n';
			gcno[sz - 1] = 'o';

			// Need a pair
			if (!file_exists(gcno) || !file_exists(gcda))
				continue;

			parseGcovFiles(gcno, gcda);
		}

		Event ev(ev_exit, WEXITSTATUS(status));

		// Report the exit status
		m_listener->onEvent(ev);

		return false;
	}

private:
	typedef std::vector<std::string> FileList_t;

	void parseGcovFiles(const std::string &gcnoFile, const std::string gcdaFile)
	{
		size_t sz;
		void *d = read_file(&sz, "%s", gcnoFile.c_str());

		if (!d)
			return;
		GcnoParser gcno((uint8_t *) d, sz);

		void *d2 = read_file(&sz, "%s", gcdaFile.c_str());

		if (!d2)
			return;
		GcdaParser gcda((uint8_t *) d2, sz);

		gcno.parse();
		gcda.parse();

		std::unordered_map<int32_t, GcnoParser::BasicBlockList_t> bbsByNumber;

		const GcnoParser::BasicBlockList_t &bbs = gcno.getBasicBlocks();
		const GcnoParser::ArcList_t &arcs = gcno.getArcs();

		for (GcnoParser::BasicBlockList_t::const_iterator it = bbs.begin(); it != bbs.end(); ++it)
		{
			const GcnoParser::BasicBlockMapping &cur = *it;

			bbsByNumber[cur.m_basicBlock].push_back(cur);
		}

		std::unordered_map<int32_t, unsigned int> counterByFunction;
		for (GcnoParser::ArcList_t::const_iterator it = arcs.begin(); it != arcs.end(); ++it)
		{
			const GcnoParser::Arc &cur = *it;

			int64_t counter = gcda.getCounter(cur.m_function, counterByFunction[cur.m_function]);

			counterByFunction[cur.m_function]++;

			// Not found?
			if (counter < 0)
			{
				warning("Arc %u but no counter\n", counterByFunction[cur.m_function]);
				continue;
			}

			// No hit, so ignore
			if (counter == 0)
				continue;

			reportBasicBlockHit(bbsByNumber[cur.m_dstBlock], counter);
			reportBasicBlockHit(bbsByNumber[cur.m_srcBlock], counter);
		}
	}

	void reportBasicBlockHit(const GcnoParser::BasicBlockList_t &bbs, int64_t counter)
	{
		for (GcnoParser::BasicBlockList_t::const_iterator it = bbs.begin(); it != bbs.end(); ++it)
		{
			const GcnoParser::BasicBlockMapping &bb = *it;

			uint64_t addr = gcovGetAddress(bb.m_file, bb.m_function, bb.m_basicBlock, bb.m_index);
			Event ev(ev_breakpoint, counter, addr);

			m_listener->onEvent(ev);
		}
	}

	// From IFileParser::IFileListener
	void onFile(const IFileParser::File &file)
	{
		if (!(file.m_flags & IFileParser::FLG_TYPE_COVERAGE_DATA))
			return;

		m_gcdaFiles.push_back(file.m_filename);
	}

	FileList_t m_gcdaFiles;
	IEventListener *m_listener;
	pid_t m_child;
};

class GcovEngineCreator : public IEngineFactory::IEngineCreator
{
public:
	virtual ~GcovEngineCreator()
	{
	}

	virtual IEngine *create(IFileParser &parser)
	{
		return new GcovEngine(parser);
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		if (!IConfiguration::getInstance().keyAsInt("gcov"))
			return match_none;

		return match_perfect;
	}
};

static GcovEngineCreator g_gcovEngineCreator;
