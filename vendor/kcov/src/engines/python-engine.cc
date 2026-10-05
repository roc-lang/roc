#include <file-parser.hh>
#include <engine.hh>
#include <configuration.hh>
#include <output-handler.hh>
#include <lineid.hh>
#include <utils.hh>
#include <source-file-cache.hh>
#include <generated-data-base.hh>

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

#include "script-engine-base.hh"

using namespace kcov;

extern GeneratedData python_helper_data;


const uint64_t COVERAGE_MAGIC = 0x6d6574616c6c6775ULL; // "metallgut"

/* Should be 8-byte aligned */
struct coverage_data
{
	uint64_t magic;
	uint32_t size;
	uint32_t line;
	const char filename[];
};

class PythonEngine : public ScriptEngineBase
{
public:
	PythonEngine() :
			ScriptEngineBase(), m_child(0), m_pipe(NULL)
	{
	}

	~PythonEngine()
	{
		kill(SIGTERM);
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		std::string command = conf.keyAsString("python-command");

		std::string kcov_python_pipe_path = IOutputHandler::getInstance().getOutDirectory() + "kcov-python.pipe";
		std::string kcov_python_path = IOutputHandler::getInstance().getBaseDirectory() + "python-helper.py";

		if (!executable_exists_in_path(command))
		{
			error("Cannot find Python parser '%s'", command.c_str());

			return false;
		}

		if (write_file(python_helper_data.data(), python_helper_data.size(), "%s", kcov_python_path.c_str()) < 0)
		{
			error("Can't write python helper at %s", kcov_python_path.c_str());

			return false;
		}

		m_listener = &listener;

		std::string kcov_python_env = "KCOV_PYTHON_PIPE_PATH=" + kcov_python_pipe_path;
		unlink(kcov_python_pipe_path.c_str());
		if (mkfifo(kcov_python_pipe_path.c_str(), 0600) < 0)
		{
			error("Can't create python FIFO %s\n", kcov_python_pipe_path.c_str());

			return false;
		}

		char *envString = (char *) xmalloc(kcov_python_env.size() + 1);
		strcpy(envString, kcov_python_env.c_str());

		putenv(envString);

		/* Launch the python helper */
		m_child = fork();
		if (m_child == 0)
		{
			const char **argv = conf.getArgv();
			unsigned int argc = conf.getArgc();

			std::string s = fmt("%s %s ", command.c_str(), kcov_python_path.c_str());
			for (unsigned int i = 0; i < argc; i++)
				s += "'" + std::string(argv[i]) + "' ";

			int res;

			res = system(s.c_str());
			panic_if(res < 0, "Can't execute python helper");

			exit(WEXITSTATUS(res));
		}
		else if (m_child < 0)
		{
			perror("fork");

			return false;
		}
		m_pipe = fopen(kcov_python_pipe_path.c_str(), "r");
		panic_if(!m_pipe, "Can't open python pipe %s", kcov_python_pipe_path.c_str());

		return true;
	}

	bool checkEvents()
	{
		uint8_t buf[8192];
		struct coverage_data *p;

		p = readCoverageDatum(buf, sizeof(buf));

		if (!p)
		{
			reportEvent(ev_error, -1);

			return false;
		}

		if (!m_reportedFiles[p->filename])
		{
			m_reportedFiles[p->filename] = true;

			for (FileListenerList_t::const_iterator it = m_fileListeners.begin(); it != m_fileListeners.end(); ++it)
				(*it)->onFile(File(p->filename, IFileParser::FLG_NONE));

			parseFile(p->filename);
		}

		if (m_listener)
		{
			uint64_t address = 0;
			Event ev;

			LineIdToAddressMap_t::const_iterator it = m_lineIdToAddress.find(getLineId(p->filename, p->line));
			if (it != m_lineIdToAddress.end())
				address = it->second;

			ev.type = ev_breakpoint;
			ev.addr = address;
			ev.data = 1;

			m_listener->onEvent(ev);
		}

		return true;
	}

	bool continueExecution()
	{
		if (checkEvents())
			return true;

		// Otherwise wait for child
		int status;
		int rv;

		rv = waitpid(m_child, &status, WNOHANG);
		if (rv != m_child)
			return true;

		if (WIFEXITED(status))
		{
			reportEvent(ev_exit_first_process, WEXITSTATUS(status));
		}
		else
		{
			warning("Other status: 0x%x\n", status);
			reportEvent(ev_error, -1);
		}

		return false;
	}

	void kill(int signal)
	{
		if (m_child == 0)
			return;

		::kill(m_child, signal);
	}

	std::string getParserType()
	{
		return "python";
	}

	unsigned int matchParser(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		std::string s((const char *) data, 80);

		if (filename.size() >= 3 && filename.substr(filename.size() - 3, filename.size()) == ".py")
			return 200;

		if (s.find("python") != std::string::npos)
			return 100;

		return match_none;
	}

private:
	void unmarshalCoverageData(struct coverage_data *p)
	{
		p->magic = be_to_host<uint64_t>(p->magic);
		p->size = be_to_host<uint32_t>(p->size);
		p->line = be_to_host<uint32_t>(p->line);
	}

	// Sweep through lines in a file to determine what is valid code
	void parseFile(const std::string &filename)
	{
		if (!m_listener)
			return;

		ISourceFileCache &cache = ISourceFileCache::getInstance();

		std::vector<std::string> lines = cache.getLines(filename);

		// Compute hash for this file
		uint32_t crc = cache.getCrc(filename);

		unsigned int lineNo = 0;
		enum
		{
			start, multiline_active
		} state = start;
		bool multiLineStartLine = false;

		for (std::vector<std::string>::const_iterator it = lines.begin(); it != lines.end(); ++it)
		{
			const std::string &s = trim_string(*it);

			lineNo++;
			// Empty line, ignore
			if (s == "")
				continue;

			// Non-empty, but comment
			if (s[0] == '#')
				continue;

			// Dict endings generate no code
			if (s == "}" || s == "},")
				continue;

			// Ditto for list ends
			if (s == "]" || s == "],")
				continue;

			// ... And starts
			if (s == "[")
				continue;

			// else: statements are nops
			if (s.find("else:") != std::string::npos)
				continue;

			// As is finally:
			if (s.find("finally:") != std::string::npos)
				continue;

			// Single-line python strings (e.g., 'description of the function')
			if (isPythonString(s))
				continue;

			size_t idx = multilineIdx(s);

			switch (state)
			{
			case start:
				if (idx != std::string::npos)
				{
					kcov_debug(ENGINE_MSG, "python multiline ON  %3d: %s\n", lineNo, s.c_str());

					std::string s2 = s.substr(idx + 3, std::string::npos);

					if (multilineIdx(s2) == std::string::npos)
						state = multiline_active;
					else
						// Ends on the same line
						break;

					/*
					 *  E.g.
					 *
					 *    a = '''yadayada [...]
					 *    hejsan
					 *    '''
					 */
					if (idx > 0)
						multiLineStartLine = true;

					// Don't report this line
					continue;
				}
				break;
			case multiline_active:
				if (idx != std::string::npos)
				{
					kcov_debug(ENGINE_MSG, "python multiline OFF %3d: %s\n", lineNo, s.c_str());
					state = start;

					// The last line of a multi-line string will get reported by the
					// python helper, so add this as a line if there was an assignment
					// above
					if (multiLineStartLine)
					{
						fileLineFound(crc, filename, lineNo);
						multiLineStartLine = false;
					}
				}
				continue; // Don't report this line
			default:
				panic("Unknown state %u", state);
				break;
			}

			fileLineFound(crc, filename, lineNo);
		}
	}

	size_t multilineIdx(const std::string &s)
	{
		size_t idx = s.find("'''");

		if (idx == std::string::npos)
			idx = s.find("\"\"\"");

		return idx;
	}

	bool isStringDelimiter(char c)
	{
		return c == '\'' || c == '"';
	}

	bool isPythonString(const std::string &s)
	{
		if (s.size() < 2)
			return false;

		char first = s[0];
		char last = s[s.size() - 1];

		if (!isStringDelimiter(first))
			return false;

		// Can be multi-line string, handled elsewhere
		if (!isStringDelimiter(last))
			return false;

		unsigned i;
		for (i = 0; i < s.size(); i++)
		{
			if (!isStringDelimiter(s[i]))
				break;
		}
		// Only string characters
		if (i == s.size())
			return false;

		return true;
	}

	struct coverage_data *readCoverageDatum(uint8_t *buf, size_t totalSize)
	{
		struct coverage_data *p = (struct coverage_data *) buf;
		ssize_t rv;

		if (feof(m_pipe))
			return NULL; // Not an error

		// No data?
		if (!file_readable(m_pipe, 100))
			return NULL;

		memset(buf, 0, sizeof(struct coverage_data));
		rv = fread(buf, 1, sizeof(struct coverage_data), m_pipe);
		if (rv == 0)
			return NULL; // Not an error

		if (rv < (int) sizeof(struct coverage_data))
		{
			error("Read too little %zd", rv);

			return NULL;
		}
		unmarshalCoverageData(p);

		bool valid = p->magic == COVERAGE_MAGIC && p->size < totalSize;

		kcov_debug(ENGINE_MSG, "datum: 0x%16llx, size %u, line %u (%svalid)\n", (unsigned long long) p->magic,
				(unsigned int) p->size, (unsigned int) p->line, valid ? "" : "in");

		if (!valid)
		{
			uint32_t *p32 = (uint32_t *) buf;

			error("Data magic wrong or size too large: %08x %08x %08x %08x\n", p32[0], p32[1], p32[2], p32[3]);

			return NULL;
		}

		size_t remainder = p->size - sizeof(struct coverage_data);
		rv = fread(buf + sizeof(struct coverage_data), 1, remainder, m_pipe);
		if (rv < (ssize_t) remainder)
		{
			error("Read too little %zd vs %zu", rv, remainder);

			return NULL;
		}

		return p;
	}

	pid_t m_child;
	FILE *m_pipe;
};

// This ugly stuff should be fixed
static PythonEngine *g_pythonEngine;
class PythonCtor
{
public:
	PythonCtor()
	{
		g_pythonEngine = new PythonEngine();
	}
};
static PythonCtor g_pythonCtor;

class PythonEngineCreator : public IEngineFactory::IEngineCreator
{
public:
	virtual ~PythonEngineCreator()
	{
	}

	virtual IEngine *create()
	{
		return g_pythonEngine;
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		std::string s((const char *) data, 80);

		if (filename.size() >= 3 && filename.substr(filename.size() - 3, filename.size()) == ".py")
			return 200;

		if (s.find("python") != std::string::npos)
			return 100;

		return match_none;
	}
};

static PythonEngineCreator g_pythonEngineCreator;
