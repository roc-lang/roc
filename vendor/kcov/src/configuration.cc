#include <configuration.hh>
#include <file-parser.hh>
#include <utils.hh>
#include <stdlib.h>
#include <getopt.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <map>
#include <list>
#include <string>
#include <stdexcept>
#include <unordered_map>
#include <iostream>
using namespace kcov;

extern "C" const char *kcov_version;

class Configuration : public IConfiguration
{
public:
	Configuration()
	{
		m_programArgs = NULL;
		m_argc = 0;
		m_printUncommon = false;

		setupDefaults();
	}

	const std::string &keyAsString(const std::string &key)
	{
		panic_if(m_strings.find(key) == m_strings.end(), "key %s not found",
				key.c_str());

		return m_strings[key];
	}

	int keyAsInt(const std::string &key)
	{
		panic_if(m_ints.find(key) == m_ints.end(), "key %s not found",
				key.c_str());

		return m_ints[key];
	}

	const std::vector<std::string> &keyAsList(const std::string &key)
	{
		panic_if(m_stringVectors.find(key) == m_stringVectors.end(),
				"key %s not found", key.c_str());

		return m_stringVectors[key];
	}

	bool usage(void)
	{
		printf(
				"Usage: kcov [OPTIONS] out-dir in-file [args...]\n"
						"\n"
						"Where [OPTIONS] are\n"
						" -h, --help              this text\n"
						" --version               print the version of kcov\n"
						" -p, --pid=PID           trace PID instead of executing in-file,\n"
						"                         in-file is optional on Linux in this case\n"
						" -l, --limits=low,high   setup limits for low/high coverage (default %u,%u)\n"
						"\n"
						" --collect-only          Only collect coverage data (don't produce HTML/\n"
						"                         Cobertura output)\n"
						" --report-only           Produce output from stored databases, don't collect\n"
						" --merge                 Merge output from multiple source dirs\n"
						"\n"
						" --include-path=path     comma-separated paths to include in the coverage report\n"
						" --exclude-path=path     comma-separated paths to exclude from the coverage\n"
						"                         report\n"
						" --include-pattern=pat   comma-separated path patterns to include in the\n"
						"                         coverage report\n"
						" --exclude-pattern=pat   comma-separated path patterns to exclude from the \n"
						"                         coverage report\n"
						" --exclude-line=pat[,...] Consider lines that match the patterns to be non-\n"
						"                         code lines.\n"
						" --exclude-region=start:stop[,...] Exclude regions of code between start:stop\n"
						"                         markers (e.g., within comments) as non-code lines.\n"
						" --clean                 don't accumulate data from multiple runs\n"
						"\n"
						" --coveralls-id=id       Travis CI job ID or secret repo_token for uploads to\n"
						"                         Coveralls.io\n"
						" --strip-path=path       If not set, max common path will be stripped away.\n"
						"%s"
						"\n"
						"Examples:\n"
						"  kcov /tmp/kcov ./frodo                        # Check coverage for ./frodo\n"
						"  kcov --pid=1000 /tmp/kcov                     # Check coverage for PID 1000\n"
						"  kcov --include-pattern=/src/frodo/ /tmp/kcov ./frodo # Only include files\n"
						"                                                       # including /src/frodo\n"
						"  kcov --collect-only /tmp/kcov ./frodo  # Collect coverage, don't report\n"
						"  kcov --report-only /tmp/kcov ./frodo   # Report coverage collected above\n"
						"  kcov --merge /tmp/out /tmp/dir1 /tmp/dir2     # Merge the dir1/dir2 reports\n"
						"  kcov --merge /tmp/out /tmp/dir*               # same as above (wildcard)\n"
						"  kcov --system-record /tmp/out-dir sysroot     # Perform full-system in-\n"
						"                                                  strumentation for sysroot\n"
						"  kcov --system-report  /tmp/data-dir           # Report all data from a full-\n"
						"                                                  system run.\n"
						"", keyAsInt("low-limit"), keyAsInt("high-limit"),
				uncommonOptions().c_str());

		return false;
	}

	void printUsage()
	{
		usage();
	}

	bool parse(unsigned int argc, const char *argv[])
	{
		static const struct option long_options[] =
		{
		{ "help", no_argument, 0, 'h' },
		{ "pid", required_argument, 0, 'p' },
		{ "limits", required_argument, 0, 'l' },
		{ "output-interval", required_argument, 0, 'O' },
		{ "path-strip-level", required_argument, 0, 'S' },
		{ "skip-solibs", no_argument, 0, 'L' },
		{ "exit-first-process", no_argument, 0, 'F' },
		{ "gcov", no_argument, 0, 'g' },
		{ "clang", no_argument, 0, 'c' },
		{ "configure", required_argument, 0, 'M' },
		{ "clean", no_argument, 0, 'E' },
		{ "cobertura-only", no_argument, 0, 'o' },
		{ "exclude-pattern", required_argument, 0, 'x' },
		{ "include-pattern", required_argument, 0, 'i' },
		{ "exclude-path", required_argument, 0, 'X' },
		{ "include-path", required_argument, 0, 'I' },
		{ "coveralls-id", required_argument, 0, 'T' },
		{ "strip-path", required_argument, 0, 'Z' },
		{ "exclude-line", required_argument, 0, 'u' },
		{ "exclude-region", required_argument, 0, 'G' },
		{ "debug", required_argument, 0, 'D' },
		{ "debug-force-bash-stderr", no_argument, 0, 'd' },
		{ "bash-handle-sh-invocation", no_argument, 0, 's' },
		{ "bash-tracefd-cloexec", no_argument, 0, 't' },
		{ "bash-dont-parse-binary-dir", no_argument, 0, 'j' },
		{ "bash-parse-files-in-dirs", required_argument, 0, 'J' },
		{ "replace-src-path", required_argument, 0, 'R' },
		{ "collect-only", no_argument, 0, 'C' },
		{ "report-only", no_argument, 0, 'r' },
		{ "merge", no_argument, 0, 'm' },
		{ "python-parser", required_argument, 0, 'P' },
		{ "patchelf", required_argument, 0, 'e' },
		{ "bash-parser", required_argument, 0, 'B' },
		{ "bash-method", required_argument, 0, '4' },
		{ "system-record", no_argument, 0, '8' },
		{ "system-report", no_argument, 0, '9' },
		{ "verify", no_argument, 0, 'V' },
		{ "version", no_argument, 0, 'v' },
		{ "uncommon-options", no_argument, 0, 'U' },
		/*{"write-file", required_argument, 0, 'w'}, Take back when the kernel stuff works */
		/*{"read-file", required_argument, 0, 'r'}, Ditto */
		{ 0, 0, 0, 0 } };
		unsigned int afterOpts = 0;
		unsigned int extraNeeded = 2;
		unsigned int lastArg;

		setupDefaults();

		const char *path = getenv("PATH");

		if (!path)
			path = "";

		std::vector<std::string> paths = split_string(path, ":");

		setKey("kcov-binary-path", get_real_path(argv[0]));

		/* Scan through the parameters for an ELF file: That will be the
		 * second last argument in the list.
		 *
		 * After that it's arguments to the external program.
		 */
		for (lastArg = 1; lastArg < argc; lastArg++)
		{
			if (IParserManager::getInstance().matchParser(get_real_path(argv[lastArg])))
				break;

			bool found = false;
			for (std::vector<std::string>::const_iterator it = paths.begin();
					it != paths.end(); ++it)
			{
				const std::string &curPath = *it;
				const std::string cur = get_real_path(
						curPath + "/" + argv[lastArg]);
				struct stat st;

				if (lstat(cur.c_str(), &st) < 0)
					continue;

				// Regular file?
				if (S_ISREG(st.st_mode) == 0)
					continue;

				// Executable?
				if ((st.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) == 0)
					continue;

				if (IParserManager::getInstance().matchParser(cur))
				{
					// Intentional memory leak
					argv[lastArg] = xstrdup(cur.c_str());
					found = true;
					break;
				}
			}

			if (found)
				break;
		}

		bool printUsage = false;

		/* Hooray for reentrancy... */
		optind = 0;
		optarg = 0;
		while (1)
		{
			int option_index = 0;
			int c;

			c = getopt_long(lastArg, (char **) argv, "hp:s:l:t:", long_options,
					&option_index);

			/* No more options */
			if (c == -1)
				break;

			switch (c)
			{
			case 0:
				break;
			case 'h':
				printUsage = true;
				break;
			case 'U':
				m_printUncommon = true;
				break;
			case 'V':
				setKey("verify", 1);
#if KCOV_HAS_LIBBFD == 0
				warning(
						"kcov: WARNING: kcov has been built without libbfd-dev (or\n"
								"kcov: binutils-dev), so the --verify option will not do anything.\n");
#endif
				break;
			case 'v':
				printf("kcov %s\n", kcov_version);
				exit(0);
				break;
			case 'L':
				setKey("parse-solibs", 0);
				break;
			case 'F':
				setKey("daemonize-on-first-process-exit", 1);
				break;
			case 'g':
				setKey("gcov", 1);
				break;
			case 'c':
				setKey("clang-sanitizer", 1);
				break;
			case 'E':
				setKey("clean-output", 1);
				break;
			case 'p':
			{
				if (!isInteger(std::string(optarg)))
					return usage();

				unsigned long pid = stoul(std::string(optarg));
				setKey("attach-pid", pid);

				// Linux can derive this from /proc
				if (file_exists(fmt("/proc/%lu/exe", pid)))
				{
					extraNeeded = 1;
				}
				break;
			}
			case 'O':
				if (!isInteger(std::string(optarg)))
					return usage();

				setKey("output-interval", stoul(std::string(optarg)));
				break;
			case 'o':
				setKey("cobertura-only", 1);
				break;
			case 'S':
			{
				if (!isInteger(std::string(optarg)))
					return usage();

				int pathStripLevel = stoul(std::string(optarg));
				if (pathStripLevel == 0)
					pathStripLevel = INT_MAX;
				setKey("path-strip-level", pathStripLevel);
				break;
			}
			case 'D':
				if (!isInteger(std::string(optarg)))
					return usage();
				g_kcov_debug_mask = stoul(std::string(optarg));
				break;
			case 'M':
			{
				std::vector<std::string> values = split_string(optarg, ",");

				for (std::vector<std::string>::const_iterator it =
						values.begin(); it != values.end(); ++it)
				{
					std::vector<std::string> keyValue = split_string(*it, "=");

					if (keyValue.size() != 2)
					{
						error("--configure needs key=value pairs\n");
						return usage();
					}

					configure(keyValue[0], keyValue[1]);
				}

				break;
			}
			case 'i':
				setKey("include-pattern",
						getCommaSeparatedList(std::string(optarg)));
				break;
			case 'P':
				setKey("python-command", optarg);
				break;
			case 'e':
				setKey("patchelf-command", optarg);
				break;
			case 'B':
				setKey("bash-command", optarg);
				break;
			case 'T':
				setKey("coveralls-id", optarg);
				break;
			case 'Z':
				setKey("strip-path", std::string(optarg));
				break;
			case 'u':
				setKey("exclude-line", std::string(optarg));
				break;
			case 'G':
				setKey("exclude-region", std::string(optarg));
				break;
			case 'x':
				setKey("exclude-pattern",
						getCommaSeparatedList(std::string(optarg)));
				break;
			case 'I':
			{
				StrVecMap_t onlyIncludePath = getCommaSeparatedList(
						std::string(optarg));
				expandPath(onlyIncludePath);

				setKey("include-path", onlyIncludePath);
				break;
			}
			case 'X':
			{
				StrVecMap_t excludePath = getCommaSeparatedList(
						std::string(optarg));
				expandPath(excludePath);

				setKey("exclude-path", excludePath);
				break;
			}
			case 'd':
				setKey("bash-force-stderr-input", 1);
				setKey("bash-use-ps4", 1); // Needed in this mode
				break;
			case 's':
				setKey("bash-handle-sh-invocation", 1);
				break;
			case 't':
				setKey("bash-tracefd-cloexec", 1);
				break;
			case '4':
			{
				std::string s(optarg);

				if (s == "DEBUG")
					setKey("bash-use-ps4", 0);
				else if (s == "PS4")
					setKey("bash-use-ps4", 1);
				else
					panic("Invalid bash method: Use PS4 or DEBUG\n");
			}
				break;
			case 'C':
				setKey("running-mode", IConfiguration::MODE_COLLECT_ONLY);
				break;
			case 'r':
				setKey("running-mode", IConfiguration::MODE_REPORT_ONLY);
				break;
			case 'm':
				setKey("running-mode", IConfiguration::MODE_MERGE_ONLY);
				break;
			case '8': // Full system record
				setKey("running-mode", IConfiguration::MODE_SYSTEM_RECORD);
				setKey("parse-solibs", 0); // Not applicable
				break;
			case '9': // Full system report
				setKey("running-mode", IConfiguration::MODE_SYSTEM_REPORT);
				setKey("parse-solibs", 0);
				break;
			case 'l':
			{
				StrVecMap_t vec = getCommaSeparatedList(std::string(optarg));

				if (vec.size() != 2)
					return usage();

				if (!isInteger(vec[0]) || !isInteger(vec[1]))
					return usage();

				setKey("low-limit", stoul(vec[0]));
				setKey("high-limit", stoul(vec[1]));
				break;
			}
			case 'j':
				setKey("bash-parse-binary-dir", 0);
				break;
			case 'J':
			{
				StrVecMap_t bashFilesInPath = getCommaSeparatedList(
						std::string(optarg));

				setKey("bash-parse-file-dir", bashFilesInPath);
				break;
			}
			case 'R':
			{
				std::string tmpArg = std::string(optarg);
				size_t tokenPosFront = tmpArg.find_first_of(":");
				size_t tokenPosBack = tmpArg.find_last_of(":");

				if ((tokenPosFront != std::string::npos)
						&& (tokenPosFront == tokenPosBack))
				{

					std::string originalPathPrefix = tmpArg.substr(0,
							tokenPosFront);
					std::string newPathPrefix = tmpArg.substr(
							tokenPosFront + 1);

					char* rp = ::realpath(newPathPrefix.c_str(), NULL);
					if (rp)
					{
						free((void*) rp);
					}
					else
					{
						panic("%s is not a valid path.\n",
								newPathPrefix.c_str());
					}

					setKey("orig-path-prefix", originalPathPrefix);
					setKey("new-path-prefix", newPathPrefix);
				}
				else
				{
					panic("%s is formatted incorrectly\n", tmpArg.c_str());

				}
				break;
			}
			default:
				error("Unrecognized option: -%c\n", optopt);
				return usage();
			}
		}

		if (printUsage)
			return usage();

		afterOpts = optind;

		/* When tracing by PID, the filename is optional */
		if (argc < afterOpts + extraNeeded)
			return usage();

		std::string outDirectory = argv[afterOpts];
		if (outDirectory[outDirectory.size() - 1] != '/')
			outDirectory += "/";

		setKey("out-directory", outDirectory);
		if (keyAsInt("running-mode") == IConfiguration::MODE_MERGE_ONLY)
		{
			// argv contains the directories to merge in this case, but we have no binary name etc
			setKey("binary-name", "merged-kcov-output");
			setKey("target-directory", outDirectory + "/merged-kcov-output");
		}
		else
		{
			std::string binaryName;
			std::string path;

			if (keyAsInt("attach-pid") == 0)
			{
				path = argv[afterOpts + 1];
			}
			else
			{
				// Trace by PID - derive from /proc/$pid/exe on Linux
				int pid = keyAsInt("attach-pid");
				path = get_real_path(fmt("/proc/%d/exe", pid));

				if (!file_exists(path))
				{
					if (argc < afterOpts + 1)
						usage();
					path = argv[afterOpts + 1];
				}
			}

			path = get_real_path(path);
			std::pair<std::string, std::string> tmp = split_path(path);

			setKey("binary-path", tmp.first);
			binaryName = tmp.second;

			if (!keyAsInt("cobertura-only"))
			{
				setKey("target-directory",
						fmt("%s/%s.%08zx", outDirectory.c_str(), binaryName.c_str(),
								(size_t)hash_file(path)));
			}
			else
			{
				// No directory structure if only cobertura is used
				setKey("target-directory", outDirectory);
			}
			if (keyAsInt("running-mode") == IConfiguration::MODE_REPORT_ONLY &&
				!file_exists(keyAsString("target-directory")))
			{
				error("report-only selected, but the target directory %s does not exist\n", keyAsString("target-directory").c_str());
				return usage();
			}

			setKey("binary-name", binaryName);

			if (keyAsString("command-name") == "")
				setKey("command-name", binaryName);
		}

		m_programArgs = &argv[afterOpts + 1];
		m_argc = argc - afterOpts - 1;

		return true;
	}

	const char **getArgv()
	{
		return m_programArgs;
	}

	unsigned int getArgc()
	{
		return m_argc;
	}

	void registerListener(IListener &listener,
			const std::vector<std::string> &keys)
	{
		for (std::vector<std::string>::const_iterator it = keys.begin();
				it != keys.end(); ++it)
			m_listeners[*it] = &listener;
	}

	// "private", but we ignore that in the unit test
	typedef std::vector<std::string> StrVecMap_t;

	typedef std::unordered_map<std::string, std::string> StringKeyMap_t;
	typedef std::unordered_map<std::string, int> IntKeyMap_t;
	typedef std::unordered_map<std::string, StrVecMap_t> StrVecKeyMap_t;

	typedef std::unordered_map<std::string, IListener *> ListenerMap_t;

	// Setup the default key:value pairs
	void setupDefaults()
	{
		setKey("kcov-binary-path", "");
		setKey("binary-path", "");
		setKey("python-command", "python");
#ifdef __FreeBSD__
		setKey("bash-command", "/usr/local/bin/bash");
#else
		setKey("bash-command", "/bin/bash");
#endif
		setKey("kernel-coverage-path", "/sys/kernel/debug/kprobe-coverage");
		setKey("path-strip-level", 2);
		setKey("attach-pid", 0);
		setKey("parse-solibs", 1);
		setKey("gcov", 0);
		setKey("clang-sanitizer", 0);
		setKey("clean-output", 0);
		setKey("low-limit", 25);
		setKey("high-limit", 75);
		setKey("output-interval", 5000);
		setKey("daemonize-on-first-process-exit", 0);
		setKey("coveralls-id", "");
		setKey("strip-path", "");
		setKey("exclude-line", "");
		setKey("exclude-region", "");
		setKey("orig-path-prefix", "");
		setKey("new-path-prefix", "");
		setKey("running-mode", IConfiguration::MODE_COLLECT_AND_REPORT);
		setKey("exclude-pattern", StrVecMap_t());
		setKey("include-pattern", StrVecMap_t());
		setKey("exclude-path", StrVecMap_t());
		setKey("include-path", StrVecMap_t());
		setKey("bash-force-stderr-input", 0);
		setKey("bash-handle-sh-invocation", 0);
		setKey("bash-tracefd-cloexec", 0);
		setKey("bash-use-basic-parser", 0);
		setKey("bash-use-ps4", 1);
		setKey("bash-parse-file-dir", StrVecMap_t());
		setKey("bash-parse-binary-dir", 1);
		setKey("verify", 0);
		setKey("command-name", "");
		setKey("merged-name", "[merged]");
		setKey("css-file", "");
		setKey("lldb-use-raw-breakpoint-writes", 0);
		setKey("system-mode-write-file", "");
		setKey("system-mode-write-file-mode", 0644);
		setKey("system-mode-read-results-file", "");
		setKey("patchelf-command", "patchelf");
		setKey("coveralls-service-name", "travis-ci");
		setKey("cobertura-full-paths", 0);
		setKey("codecov-full-paths", 0);
		setKey("cobertura-only", 0);
		setKey("is-go-binary", 0);
	}

	void setKey(const std::string &key, const std::string &val)
	{
		m_strings[key] = val;

		notifyKeyListeners(key);
	}

	void setKey(const std::string &key, int val)
	{
		m_ints[key] = val;

		notifyKeyListeners(key);
	}

	void setKey(const std::string &key, const std::vector<std::string> &val)
	{
		m_stringVectors[key] = val;

		notifyKeyListeners(key);
	}

	void notifyKeyListeners(const std::string &key)
	{
		ListenerMap_t::iterator it = m_listeners.find(key);

		if (it == m_listeners.end())
			return;

		// The callback should re-read the configuration item
		it->second->onConfigurationChanged(key);
	}

	void configure(const std::string &key, const std::string &value)
	{
		if (key == "low-limit" || key == "high-limit"
				|| key == "bash-use-basic-parser"
				|| key == "cobertura-full-paths"
				|| key == "codecov-full-paths")
		{
			if (!isInteger(value))
				panic("Value for %s must be integer\n", key.c_str());
		}

		if (key == "low-limit")
			setKey(key, stoul(std::string(value)));
		else if (key == "high-limit")
			setKey(key, stoul(std::string(value)));
		else if (key == "system-mode-write-file")
			setKey(key, std::string(value));
		else if (key == "system-mode-read-results-file")
			setKey(key, stoul(std::string(value)));
		else if (key == "bash-use-basic-parser")
			setKey(key, stoul(std::string(value)));
		else if (key == "lldb-use-raw-breakpoint-writes")
			setKey(key, stoul(std::string(value)));
		else if (key == "command-name")
			setKey(key, std::string(value));
		else if (key == "css-file")
			setKey(key, std::string(value));
		else if (key == "merged-name")
			setKey(key, std::string(value));
		else if (key == "coveralls-service-name")
			setKey(key, std::string(value));
		else if (key == "cobertura-full-paths")
			setKey(key, stoul(std::string(value)));
		else if (key == "codecov-full-paths")
			setKey(key, stoul(std::string(value)));
		else
			panic("Unknown key %s\n", key.c_str());
	}

	const char *getConfigurableValues()
	{
		return "                           bash-use-basic-parser=1    Enable simple bash parser\n"
				"                           command-name=STR           Name of executed command\n"
				"                           css-file=FILE              Filename of bcov.css file\n"
				"                           high-limit=NUM             Percentage for high coverage\n"
				"                           low-limit=NUM              Percentage for low coverage\n"
				"                           merged-name=STR            Name of [merged] tag in HTML\n"
				"                           coveralls-service-name=STR Service name for coveralls\n";
	}

	std::string uncommonOptions()
	{
		if (!m_printUncommon)
			return " --uncommon-options      print uncommon options for --help\n";

		return fmt(
				" --replace-src-path=path replace the string found before the : with the string \n"
						"                         found after the :\n"
						" --path-strip-level=num  path levels to show for common paths (default: %d)\n"
						"\n"
						" --cobertura-only        Only produce cobertura output in the output dir\n"
						" --gcov                  use gcov parser instead of DWARF debugging info\n"
						" --clang                 use Clang Sanitizer-coverage parser\n"
						" --system-record         perform full-system instrumentation\n"
						" --system-report         report full-system coverage data\n"
						" --skip-solibs           don't parse shared libraries (default: parse solibs)\n"
						" --exit-first-process    exit when the first process exits, i.e., honor the\n"
						"                         behavior of daemons (default: wait until last)\n"
						" --output-interval=ms    Interval to produce output in milliseconds (0 to\n"
						"                         only output when kcov terminates, default %d)\n"
						"\n"
						" --debug=X               set kcov debugging level (max 31, default 0)\n"
						"\n"
						" --configure=key=value,... Manually set configuration values. Possible values:\n"
						"%s"
						"\n"
						" --verify                verify breakpoint setup (to catch compiler bugs)\n"
						"\n"
						" --python-parser=cmd     Python parser to use (for python script coverage),\n"
						"                         default: %s\n"
						" --bash-parser=cmd       Bash parser to use (for bash/sh script coverage),\n"
						"                         default: %s\n"
						" --patchelf=cmd          patchelf binary to use (for system mode),\n"
						"                         default: %s\n"
						" --bash-method=method    Bash coverage collection method, PS4 (default) or DEBUG\n"
						" --bash-handle-sh-invocation  Try to handle #!/bin/sh scripts by a LD_PRELOAD\n"
						"                         execve replacement. Buggy on some systems\n"
						" --bash-tracefd-cloexec  Force children to not be traced by configuring the trace\n"
						"                         fd as non-cloneable with LD_PRELOAD. Buggy on some systems\n"
						" --bash-dont-parse-binary-dir Don't parse the binary directory for other scripts\n"
						" --bash-parse-files-in-dir=dir[,...]  Parse bash scripts in dir(s)\n",
				keyAsInt("path-strip-level"), keyAsInt("output-interval"),
				getConfigurableValues(), keyAsString("python-command").c_str(),
				keyAsString("bash-command").c_str(),
				keyAsString("patchelf-command").c_str());
	}

	bool isInteger(std::string str)
	{
		size_t pos;

		try
		{
			stoul(str, &pos);
		} catch (std::invalid_argument &e)
		{
			return false;
		}

		return pos == str.size();
	}

	void expandPath(StrVecMap_t &paths)
	{
		for (StrVecMap_t::iterator it = paths.begin(); it != paths.end(); ++it)
		{
			std::string &s = *it;

			if (s[0] == '~')
				s = get_home() + s.substr(1, s.size());
			*it = s;
		}
	}

	StrVecMap_t getCommaSeparatedList(std::string str)
	{
		StrVecMap_t out;

		if (str.find(',') == std::string::npos)
		{
			out.push_back(str);
			return out;
		}

		size_t pos, lastPos;

		lastPos = 0;
		for (pos = str.find_first_of(","); pos != std::string::npos;
				pos = str.find_first_of(",", pos + 1))
		{
			std::string cur = str.substr(lastPos, pos - lastPos);

			out.push_back(cur);
			lastPos = pos + 1;
		}
		out.push_back(str.substr(lastPos, str.size() - lastPos));

		return out;
	}

	StringKeyMap_t m_strings;
	IntKeyMap_t m_ints;
	StrVecKeyMap_t m_stringVectors;

	const char **m_programArgs;
	unsigned int m_argc;
	bool m_printUncommon;

	ListenerMap_t m_listeners;
};

IConfiguration & IConfiguration::getInstance()
{
	static Configuration *g_instance;

	if (!g_instance)
		g_instance = new Configuration();

	return *g_instance;
}
