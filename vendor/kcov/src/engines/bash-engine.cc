#define _WITH_GETLINE	// Needed on FreeBSD < 12.0
#include <file-parser.hh>
#include <engine.hh>
#include <configuration.hh>
#include <output-handler.hh>
#include <lineid.hh>
#include <utils.hh>
#include <generated-data-base.hh>
#include <source-file-cache.hh>

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <signal.h>
#include <dirent.h>
#include <stdio.h>

#include <list>
#include <unordered_map>
#include <vector>
#include <algorithm>

#include <swap-endian.hh>

#include "script-engine-base.hh"

using namespace kcov;

extern GeneratedData bash_helper_data;
extern GeneratedData bash_helper_debug_trap_data;
extern GeneratedData bash_redirector_library_data;
extern GeneratedData bash_cloexec_library_data;

enum InputType
{
	INPUT_NORMAL,       // No multiline stuff
	INPUT_QUOTE,        // "
	INPUT_SINGLE_QUOTE, // '
	INPUT_MULTILINE,    // backslash backslash
};

class BashEngine : public ScriptEngineBase
{
public:
	BashEngine() :
			ScriptEngineBase(), m_child(0), m_stderr(NULL), m_stdout(NULL), m_bashSupportsXtraceFd(false),
			m_inputType(INPUT_NORMAL)
	{
	}

	~BashEngine()
	{
		kill(SIGTERM);
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		int stderrPipe[2];
		int stdoutPipe[2];

		if (pipe(stderrPipe) < 0)
		{
			error("Failed to open pipe");

			return false;
		}

		if (pipe(stdoutPipe) < 0)
		{
			error("Failed to open pipe");
			close(stderrPipe[0]);
			close(stderrPipe[1]);

			return false;
		}

		m_bashSupportsXtraceFd = bashCanHandleXtraceFd();

		std::string helperPath = IOutputHandler::getInstance().getBaseDirectory() + "bash-helper.sh";
		std::string helperDebugTrapPath = IOutputHandler::getInstance().getBaseDirectory() + "bash-helper-debug-trap.sh";
		std::string redirectorPath = IOutputHandler::getInstance().getBaseDirectory() + "libbash_execve_redirector.so";
		std::string cloexecPath = IOutputHandler::getInstance().getBaseDirectory() + "libbash_tracefd_cloexec.so";

		if (write_file(bash_helper_data.data(), bash_helper_data.size(), "%s", helperPath.c_str()) < 0)
		{
			error("Can't write helper");

			return false;
		}
		if (write_file(bash_helper_debug_trap_data.data(), bash_helper_debug_trap_data.size(), "%s",
				helperDebugTrapPath.c_str()) < 0)
		{
			error("Can't write helper");

			return false;
		}
		if (write_file(bash_redirector_library_data.data(), bash_redirector_library_data.size(), "%s",
				redirectorPath.c_str()) < 0)
		{
			error("Can't write redirector library at %s", redirectorPath.c_str());

			return false;
		}

		if (write_file(bash_cloexec_library_data.data(), bash_cloexec_library_data.size(), "%s",
				cloexecPath.c_str()) < 0)
		{
			error("Can't write tracefd-cloexec library at %s", cloexecPath.c_str());

			return false;
		}

		m_listener = &listener;

		/* Launch child */
		m_child = fork();
		if (m_child > 0)
		{

			// Close the parents write end of the pipe
			close(stderrPipe[1]);
			close(stdoutPipe[1]);

			// And open a FILE * to stderr
			m_stderr = fdopen(stderrPipe[0], "r");
			if (!m_stderr)
			{
				error("Can't reopen the stderr pipe");
				return false;
			}
			m_stdout = fdopen(stdoutPipe[0], "r");
			if (!m_stdout)
			{
				error("Can't reopen the stdout pipe");
				fclose(m_stderr);

				return false;
			}
			// If xtrace fd is supported, make m_stdout
			// non blocking to drain it as soon as data
			// are available (without waiting for \n)
			if (m_bashSupportsXtraceFd)
				make_file_non_blocking(m_stdout);

		}
		else if (m_child == 0)
		{
			const char **argv = conf.getArgv();
			unsigned int argc = conf.getArgc();
			int xtraceFd = 782; // Typical bash users use 3,4 etc but not high fd numbers (?)
			{
				// However, the range of fd numbers that this process can use is limited by the caller of this process.
				// The following code specification will require discussion with many kcov users.
				struct rlimit rlim;
				if (getrlimit(RLIMIT_NOFILE, &rlim) < 0) {
					perror("Failed to get the maximum number of open file descriptors.");
					return false;
				}
				xtraceFd = rlim.rlim_cur / 4;
			}
			const std::string command = conf.keyAsString("bash-command");
			bool usePS4 = conf.keyAsInt("bash-use-ps4");

			// Revert to stderr if this bash version can't handle BASH_XTRACE
			if (usePS4 && !m_bashSupportsXtraceFd)
				xtraceFd = 2;

			if (dup2(stderrPipe[1], xtraceFd) < 0)
			{
				perror("Failed to exchange stderr for pipe");
				return false;
			}
			if (dup2(stdoutPipe[1], 1) < 0)
			{
				perror("Failed to exchange stderr for pipe");
				return false;
			}

			/* Close the childs old write end of the pipe */
			close(stderrPipe[1]);
			close(stdoutPipe[1]);

			/* Close the childs read end of the pipe */
			close(stderrPipe[0]);
			close(stdoutPipe[0]);

			doSetenv(fmt("KCOV_BASH_XTRACEFD=%d", xtraceFd));
			// Export the bash command for use in the bash-execve-redirector library
			doSetenv(fmt("KCOV_BASH_COMMAND=%s", command.c_str()));

			/* Set up PS4 for tracing */
			if (usePS4)
			{
				doSetenv(fmt("BASH_ENV=%s", helperPath.c_str()));
				doSetenv(fmt("BASH_XTRACEFD=%d", xtraceFd));
			}
			else
			{
				// Use DEBUG trap
				doSetenv(fmt("BASH_ENV=%s", helperDebugTrapPath.c_str()));
				doSetenv(fmt("KCOV_BASH_USE_DEBUG_TRAP=1"));
			}

			// And preload it!
			if (conf.keyAsInt("bash-handle-sh-invocation") ||
			    conf.keyAsInt("bash-tracefd-cloexec"))
			{
				std::string preloadStr;
				if (conf.keyAsInt("bash-handle-sh-invocation"))
						preloadStr += redirectorPath;
				if (conf.keyAsInt("bash-tracefd-cloexec"))
						preloadStr += (preloadStr.size() ? ":" : "") + cloexecPath;

				doSetenv(std::string("LD_PRELOAD=" + preloadStr));
                        }

			// Make a copy of the vector, now with "bash -x" first
			char **vec;
			int argcStart = 1;
			vec = (char **) xmalloc(sizeof(char *) * (argc + 3));
			vec[0] = xstrdup(conf.keyAsString("bash-command").c_str());

			for (unsigned i = 0; i < argc; i++)
				vec[argcStart + i] = xstrdup(argv[i]);

			/* Execute the script */
			if (execv(vec[0], vec))
			{
				perror("Failed to execute script");

				free(vec);

				return false;
			}
		}
		else if (m_child < 0)
		{
			perror("fork");

			return false;
		}

		std::vector<std::string> dirsToParse = conf.keyAsList("bash-parse-file-dir");

		for (std::vector<std::string>::iterator it = dirsToParse.begin(); it != dirsToParse.end(); ++it)
		{
			parseDirectoryForFiles(*it);
		}

		// Parse the directory where the script-under-test is for other bash scripts
		if (conf.keyAsInt("bash-parse-binary-dir"))
		{
			parseDirectoryForFiles(conf.keyAsString("binary-path"));
		}
		parseFile(executable);

		return true;
	}

	bool checkEvents()
	{
		char *curLine = NULL;
		ssize_t len;
		size_t linecap = 0;

		// First printout any collected stdout data
		handleStdout();

		len = getline(&curLine, &linecap, m_stderr);
		if (len < 0)
			return false;

		std::string cur(curLine);
		// Line markers always start with kcov@

		if (m_inputType == INPUT_SINGLE_QUOTE) {
			m_inputType = getInputType(cur);
			return true;
		}

		size_t kcovStr = cur.find("kcov@");
		if (kcovStr == std::string::npos)
		{
			fprintf(stderr, "%s", cur.c_str());
			return true;
		}
		m_inputType = getInputType(cur);

		std::vector<std::string> parts = split_string(cur.substr(kcovStr), "@");

		// kcov@FILENAME@LINENO@...
		if (parts.size() < 3)
			return true;

		// Resolve filename (might be relative)
		const std::string &filename = get_real_path(parts[1]);
		const std::string &lineNo = parts[2];

		// Skip the helper libraries
		if (filename.find("bash-helper.sh") != std::string::npos)
			return true;
		if (filename.find("bash-helper-debug-trap.sh") != std::string::npos)
			return true;

		if (!string_is_integer(lineNo))
		{
			error("%s is not an integer", lineNo.c_str());

			return false;
		}

		if (!m_reportedFiles[filename])
		{
			m_reportedFiles[filename] = true;

			for (FileListenerList_t::const_iterator it = m_fileListeners.begin(); it != m_fileListeners.end(); ++it)
				(*it)->onFile(File(filename, IFileParser::FLG_NONE));

			parseFile(filename);
		}

		if (m_listener)
		{
			uint64_t address = 0;
			Event ev;

			LineIdToAddressMap_t::iterator it = m_lineIdToAddress.find(getLineId(filename, string_to_integer(lineNo)));
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
		{
			return true;
		}
		else
		{
			// Child exited, let's make sure that we have all the events/stdout.
			checkEvents();
		}

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
		return "bash";
	}

	unsigned int matchParser(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		std::string s((const char *) data, 80);

		if (filename.size() >= 3 && filename.substr(filename.size() - 3, filename.size()) == ".sh")
			return 200;

		if (s.find("#!/bin/bash") != std::string::npos)
			return 400;
		if (s.find("#!/bin/sh") != std::string::npos)
			return 300;
		if (s.find("bash") != std::string::npos)
			return 100;
		if (s.find("sh") != std::string::npos)
			return 50;

		return match_none;
	}

private:
	void parseDirectoryForFiles(const std::string &base)
	{
		DIR *dir = ::opendir(base.c_str());
		if (!dir)
		{
			error("Can't open directory %s\n", base.c_str());
			return;
		}

		// Loop through the directory structure
		struct dirent *de;
		for (de = ::readdir(dir); de; de = ::readdir(dir))
		{
			std::string cur = base + "/" + de->d_name;

			if (strcmp(de->d_name, ".") == 0)
				continue;

			if (strcmp(de->d_name, "..") == 0)
				continue;

			struct stat st;

			if (lstat(cur.c_str(), &st) < 0)
				continue;

			if (S_ISDIR(st.st_mode))
			{
				parseDirectoryForFiles(cur);
			}
			else
			{
				size_t sz;
				uint8_t *p = (uint8_t *) read_file(&sz, "%s", cur.c_str());

				if (p)
				{
					// Bash file?
					if (matchParser(cur, p, sz) != match_none)
					{
						parseFile(cur);
					}

				}
				free((void *) p);
			}
		}
		::closedir(dir);
	}

	// Printout lines to stdout, except kcov markers
	void handleStdout()
	{
		// If we have xtrace fd support, no kcov marker
		// is present in stdout. So there is no need to
		// parse it, we can just drain it.
		if (m_bashSupportsXtraceFd)
		{
			const int size = 4096;
			static char drainedOutput[size];

			clearerr(m_stdout);
			while (!feof(m_stdout) && !ferror(m_stdout) && file_readable(m_stdout, 0))
			{
				size_t len = fread(drainedOutput, 1, size, m_stdout);
				if (len)
					fwrite(drainedOutput, 1, len, stdout);
			}
			return;
		}

		char *curLine = NULL;
		size_t linecap = 0;

		while (file_readable(m_stdout, 0) && getline(&curLine, &linecap, m_stdout) > 0)
		{
			/* Check for line markers to filter these away.
			 *
			 * For some reason, redirection sometimes give kkcov@..., so filter that
			 * in addition to the obvious stuff
			 */
			size_t kcovMarker = std::string(curLine).find("kcov@");

			if (kcovMarker != 0 && kcovMarker != 1)
			{
				printf("%s", curLine);
			}
		}
		free(curLine);
	}

	bool bashCanHandleXtraceFd()
	{
		FILE *fp;
		bool out = false;
		IConfiguration &conf = IConfiguration::getInstance();
		std::string cmd = conf.keyAsString("bash-command") + " -c 'echo \"$BASH_VERSION\"'";

		/* Has input via stderr been forced regardless of bash version?
		 * Basically for testing only
		 */
		if (conf.keyAsInt("bash-force-stderr-input"))
			return false;

		fp = popen(cmd.c_str(), "r");
		if (!fp)
			return false;

		int major = 0;
		int minor = 0;

		// Read the first line
		if (fscanf(fp, "%d.%d", &major, &minor) != EOF) {
			// Bash 4.2 and above supports BASH_XTRACEFD
			if (major > 4 || (major == 4 && minor >= 2))
				out = true;
		}

		pclose(fp);

		return out;
	}

	enum InputType getInputType(const std::string &str)
	{
		enum InputType out = m_inputType;

		for(std::string::size_type i = 0; i < str.size(); ++i) {
			if (str[i] == '\\' && out == INPUT_NORMAL)
				i++;
			else if (str[i] == '\'')
				out = (out == INPUT_NORMAL ? INPUT_SINGLE_QUOTE : INPUT_NORMAL);
		}

		return out;
	}

	void doSetenv(const std::string &val)
	{
		// "Leak" some bytes, but the environment needs that
		char *envString = (char *) xmalloc(val.size() + 1);

		strcpy(envString, val.c_str());

		putenv(envString);
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

		IConfiguration &conf = IConfiguration::getInstance();

		if (conf.keyAsInt("bash-use-basic-parser"))
			parseFileBasic(filename, lines, crc);
		else
			parseFileFull(filename, lines, crc);
	}

	void parseFileBasic(const std::string &filename, const std::vector<std::string> &lines, uint32_t crc)
	{
		unsigned int lineNo = 0;

		for (std::vector<std::string>::const_iterator it = lines.begin(); it != lines.end(); ++it)
		{
			std::string s = trim_string(*it);

			lineNo++;

			kcov_debug(ENGINE_MSG, "bash_line %2d: %s\n", lineNo, s.c_str());

			// Remove comments
			size_t comment = s.find("#");
			if (comment != std::string::npos)
			{

				// But not $# or ${#variable}...
				if ((comment >= 1 && s[comment - 1] == '$')
						|| (comment >= 2 && s.rfind("${", comment) != std::string::npos
								&& s.find("}", comment) != std::string::npos))
				{
					// Do nothing
					// Nor if in a string
				}
				else if (comment >= 1 && (s.find("\"") < comment || s.find("'") < comment))
				{
					// Do nothing
				}
				else
				{
					s = trim_string(s);
					s = s.substr(0, comment);
				}
			}

			// Empty line, ignore
			if (s == "")
				continue;

			// While, if, switch endings
			if (s == "esac" || s == "fi" || s == "do" || s == "done" || s == "else" || s == "then" || s == "}" || s == "{")
				continue;

			// Functions
			if (s.find("function") == 0)
				continue;

			fileLineFound(crc, filename, lineNo);
		}
	}

	void parseFileFull(const std::string &filename, const std::vector<std::string> &lines, uint32_t crc)
	{
		unsigned int lineNo = 0;
		enum
		{
			none, backslash, quote, heredoc
		} state = none;
		bool caseActive = false;
		bool arithmeticActive = false;
		std::string heredocMarker;

		for (std::vector<std::string>::const_iterator it = lines.begin(); it != lines.end(); ++it)
		{
			std::string s = trim_string(*it);

			lineNo++;

			kcov_debug(ENGINE_MSG, "bash_line %2d: %s\n", lineNo, s.c_str());

			// Remove comments
			size_t comment = s.find("#");
			if (comment != std::string::npos)
			{

				// But not $# or ${#variable}...
				if ((comment >= 1 && s[comment - 1] == '$')
						|| (comment >= 2 && s.rfind("${", comment) != std::string::npos
								&& s.find("}", comment) != std::string::npos))
				{
					// Do nothing
					// Nor if in a string
				}
				else if (comment >= 1 && (s.find("\"") < comment || s.find("'") < comment))
				{
					// Do nothing
				}
				else
				{
					s = trim_string(s);
					s = s.substr(0, comment);
				}
			}

			// Empty line, ignore
			if (s == "")
				continue;

			if (s.size() >= 2 && s[0] == ';' && s[1] == ';')
				continue;

			// Empty braces
			if ((s[0] == '{' || s[0] == '}') && s.size() == 1)
				continue;

			// While, if, switch endings
			if (s == "esac" || s == "fi" || s == "do" || s == "done" || s == "else" || s == "then")
				continue;

			// Functions
			if (s.find("function") == 0)
				continue;

			// fn() { .... Yes, regexes would have been better
			size_t fnPos = s.find("()");
			if (fnPos != std::string::npos)
			{
				// Trim valid last spaces between function name and parenthesis
				while (fnPos > 1 && ' ' == s[fnPos - 1])
				{
					--fnPos;
				}

				std::string substr = s.substr(0, fnPos);

				// Should be a single word (the function name)
				if (substr.find(" ") == std::string::npos)
					continue;
			}

			// Handle backslashes - only the first line is code
			if (state == backslash)
			{
				if (s[s.size() - 1] != '\\')
					state = none;
				continue;
			}

			// Multi-line quote - only the last line is code
			if (state == quote)
			{
				if (s.find('"') == s.size() - 1) // String ends with "
					state = none;
				else
					continue;
			}

			// HERE documents
			if (state == heredoc)
			{
				std::string trimmed = trim_string(s, " \t\r\n`");

				if (trimmed == heredocMarker)
					state = none;
				continue;
			}

			if (s.find("$((") != std::string::npos || s.find("$[") != std::string::npos)
				arithmeticActive = true;

			if (s[s.size() - 1] == '\\')
			{
				state = backslash;
			}
			else if ((s.find("=\"") != std::string::npos || // Handle multi-line string assignments
					s.find("= \"") != std::string::npos) && std::count(s.begin(), s.end(), '"') == 1)
			{
				state = quote;
				continue;
			}
			else
			{
				size_t heredocStart = s.find("<<");

				if (!arithmeticActive && s.find("let ") != 0 && s.find("$((") == std::string::npos
						&& s.find("))") == std::string::npos && heredocStart != std::string::npos)
				{
					// Skip << and remove spaces before and after "EOF"
					heredocMarker = trim_string(s.substr(heredocStart + 2, s.size()));

					// Make sure the heredoc marker is a word
					for (unsigned int i = 0; i < heredocMarker.size(); i++)
					{
						if (heredocMarker[i] == ' ' || heredocMarker[i] == '\t')
						{
							heredocMarker = heredocMarker.substr(0, i);
							break;
						}
					}

					if (heredocMarker[0] == '-')
					{
						// '-' marks tab-suppression in heredoc
						heredocMarker = heredocMarker.substr(1);
					}

					if (heredocMarker.length() > 2 && (heredocMarker[0] == '"' || heredocMarker[0] == '\'')
							&& heredocMarker[0] == heredocMarker[heredocMarker.length() - 1])
					{
						// remove enclosing in quotes
						heredocMarker = heredocMarker.substr(1, heredocMarker.length() - 2);
					}

					if (heredocMarker.size() > 0 && heredocMarker[0] != '<')
						state = heredoc;

					std::string beforeHeredoc = trim_string(s.substr(0, heredocStart));
					/*
					 * Special case for things like
					 * while read -r line ; do
					 *     echo $line
					 * done <<EOF
					 * xxx
					 * yyy
					 * EOF
					 *
					 * where the done statement is nocode (as above)
					 */
					if (beforeHeredoc == "done")
						continue;
				}
			}

			if (s.find("case") == 0)
				caseActive = true;
			else if (s.find("esac") == 0)
				caseActive = false;

			if (s.find("))") != std::string::npos || s.find("]") != std::string::npos)
				arithmeticActive = false;

			// Only the last line of arithmetic is code
			if (arithmeticActive)
				continue;

			// Case switches are nocode
			if (caseActive && s[s.size() - 1] == ')')
			{
				// But let functions be (albeit not with balanced parentheses)
				size_t startParenthesis = s.find("(");
				if (startParenthesis == std::string::npos || startParenthesis == 0)
					continue;
			}

			fileLineFound(crc, filename, lineNo);
		}
	}

	pid_t m_child;
	FILE *m_stderr;
	FILE *m_stdout;
	bool m_bashSupportsXtraceFd;
	enum InputType m_inputType;
};

// This ugly stuff should be fixed
static BashEngine *g_bashEngine;
class BashCtor
{
public:
	BashCtor()
	{
		g_bashEngine = new BashEngine();
	}
};
static BashCtor g_bashCtor;

class BashEngineCreator: public IEngineFactory::IEngineCreator
{
public:
	virtual ~BashEngineCreator()
	{
	}

	virtual IEngine *create()
	{
		return g_bashEngine;
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		std::string s((const char *) data, 80);

		if (filename.size() >= 3 && filename.substr(filename.size() - 3, filename.size()) == ".sh")
			return 200;

		if (s.find("#!/bin/bash") != std::string::npos)
			return 400;
		if (s.find("#!/bin/sh") != std::string::npos)
			return 300;
		if (s.find("bash") != std::string::npos)
			return 100;
		if (s.find("sh") != std::string::npos)
			return 50;

		return match_none;
	}
};

static BashEngineCreator g_bashEngineCreator;
