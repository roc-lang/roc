#include <configuration.hh>
#include <engine.hh>
#include <reporter.hh>
#include <writer.hh>
#include <collector.hh>
#include <filter.hh>
#include <output-handler.hh>
#include <file-parser.hh>
#include <solib-handler.hh>
#include <generated-data-base.hh>
#include <utils.hh>

#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <unistd.h>

#include "merge-parser.hh"
#include "engines/system-mode-file-format.hh"
#include "writers/codecov-writer.hh"
#include "writers/html-writer.hh"
#include "writers/json-writer.hh"
#include "writers/coveralls-writer.hh"
#include "writers/cobertura-writer.hh"
#include "writers/sonarqube-xml-writer.hh"

using namespace kcov;

extern GeneratedData kcov_system_library_data;

static IEngine *g_engine;
static IOutputHandler *g_output;
static ICollector *g_collector;
static IReporter *g_reporter;
static ISolibHandler *g_solibHandler;
static IFilter *g_filter;
static IFilter *g_basicFilter;

static void do_cleanup()
{
	delete g_collector;
	delete g_output;
	delete g_reporter;
	delete g_solibHandler; // Before the engine since a SIGTERM is sent to the thread
	delete g_engine;
	delete g_filter;
	delete g_basicFilter;
}

static void ctrlc(int sig)
{
	// Forward the signal to the traced program
	g_engine->kill(sig);
}

static void daemonize(void)
{
	pid_t child;
	int res;

	IConfiguration &conf = IConfiguration::getInstance();
	std::string fifoName = conf.keyAsString("target-directory") + "/done.fifo";

	unlink(fifoName.c_str());
	res = mkfifo(fifoName.c_str(), 0600);

	panic_if(res < 0, "Can't create FIFO");

	child = fork();

	if (child < 0)
	{
		panic("Fork failed?\n");
	}
	else if (child == 0)
	{
		child = fork();

		if (child < 0)
		{
			panic("Fork failed?\n");
		}
		else if (child > 0)
		{
			// Second parent
			exit(0);
		}
	}
	else
	{
		// Parent
		FILE *fp;
		char buf[255];
		char *endp;
		char *p;
		unsigned long out;

		fp = fopen(fifoName.c_str(), "r");
		panic_if(!fp, "Can't open FIFO");
		p = fgets(buf, sizeof(buf), fp);
		panic_if(!p, "Can't read FIFO");

		out = strtoul(p, &endp, 10);
		if (p == endp)
			out = 0;

		exit(out);
	}
}

// Return the number of metadata directories in the kcov output path
unsigned int countMetadata()
{
	IConfiguration &conf = IConfiguration::getInstance();
	std::string base = conf.keyAsString("out-directory");
	DIR *dir;
	struct dirent *de;

	dir = ::opendir(base.c_str());
	if (!dir)
		return 0;

	unsigned int out = 0;

	// Count metadata directories
	for (de = ::readdir(dir); de; de = ::readdir(dir))
	{
		std::string name = de->d_name;
		std::string cur = base + name + "/metadata";
		std::string binaryName = conf.keyAsString("binary-name");

		// ... except for the current coveree and coveree.CHECKSUM
		if (binaryName == name
				|| (name.size() > binaryName.size()
						&& name.find(binaryName) == 0
						&& name[binaryName.size()] == '.'))
			continue;

		DIR *metadataDir;
		struct dirent *de2;

		metadataDir = ::opendir(cur.c_str());
		if (!metadataDir)
			continue;

		// Count metadata files
		unsigned int datum = 0;
		for (de2 = ::readdir(metadataDir); de2; de2 = ::readdir(metadataDir))
		{
			if (de2->d_name[0] == '.')
				continue;

			datum++;
		}
		out += !!datum;
		::closedir(metadataDir);
	}
	::closedir(dir);

	return out;
}

/*
 * Run in merge-mode, i.e., run kcov on previously generated coverage runs and
 * create a common merged output from them.
 */
static int runMergeMode()
{
	IFilter &filter = IFilter::create();
	IReporter &reporter = IReporter::createDummyReporter();
	IOutputHandler &output = IOutputHandler::create(reporter, NULL);
	IConfiguration &conf = IConfiguration::getInstance();

	const std::string &base = output.getBaseDirectory();
	const std::string &out = output.getOutDirectory();

	IMergeParser &mergeParser = createMergeParser(reporter, base, out, filter);
	IReporter &mergeReporter = IReporter::create(mergeParser, mergeParser,
			filter);
	IWriter &mergeHtmlWriter = createHtmlWriter(mergeParser, mergeReporter,
			base, base + "/kcov-merged", conf.keyAsString("merged-name"), true);
	IWriter &mergeJsonWriter = createJsonWriter(mergeParser, mergeReporter,
			base + "kcov-merged/coverage.json");
	IWriter &mergeCoberturaWriter = createCoberturaWriter(mergeParser,
			mergeReporter, base + "kcov-merged");
	IWriter &mergeSonarqubeWriter = createSonarqubeWriter(mergeParser,
			mergeReporter, base + "kcov-merged/sonarqube.xml");
	IWriter &mergeCoverallsWriter = createCoverallsWriter(mergeParser,
			mergeReporter);
	IWriter &mergeCodecovWriter = createCodecovWriter(mergeParser, mergeReporter,
			base + "kcov-merged/codecov.json");
	(void) mkdir(fmt("%s/kcov-merged", base.c_str()).c_str(), 0755);

	output.registerWriter(mergeParser);
	output.registerWriter(mergeHtmlWriter);
	output.registerWriter(mergeJsonWriter);
	output.registerWriter(mergeCoberturaWriter);
	output.registerWriter(mergeSonarqubeWriter);
	output.registerWriter(mergeCoverallsWriter);
	output.registerWriter(mergeCodecovWriter);

	output.start();
	output.stop();

	mergeReporter.writeCoverageDatabase();
	delete &output;

	return 0;
}

static int runKcov(IConfiguration::RunMode_t runningMode)
{
	IConfiguration &conf = IConfiguration::getInstance();

	std::string file = conf.keyAsString("binary-path")
			+ conf.keyAsString("binary-name");
	IFileParser *parser = IParserManager::getInstance().matchParser(file);
	if (!parser)
	{
		error("Can't find or open %s\n", file.c_str());
		return 1;
	}

	if ((parser->getParserType() == "bash" ||
		parser->getParserType() == "python") &&
		!conf.keyAsInt("cobertura-only"))
	{
		// Use the path as the hash for non-compiled languages
		conf.setKey("target-directory",
				fmt("%s/%s.%08zx", conf.keyAsString("out-directory").c_str(), conf.keyAsString("binary-name").c_str(),
				std::hash<std::string>()(conf.keyAsString("binary-name").c_str())));
	}

	// Match and create an engine
	IEngineFactory::IEngineCreator &engineCreator =
			IEngineFactory::getInstance().matchEngine(file);
	IEngine *engine = engineCreator.create();
	if (!engine)
	{
		conf.printUsage();
		return 1;
	}

	IFilter &filter = IFilter::create();
	IFilter &basicFilter = IFilter::createBasic();

	ICollector &collector = ICollector::create(*parser, *engine, filter);
	IReporter &reporter = IReporter::create(*parser, collector, filter);
	IOutputHandler &output = IOutputHandler::create(reporter, &collector);
	ISolibHandler &solibHandler = createSolibHandler(*parser, collector);

	parser->addFile(file);

	const std::string &base = output.getBaseDirectory();
	const std::string &out = output.getOutDirectory();
	// The merge parser is both a parser, a writer and a collector (!)
	IMergeParser &mergeParser = createMergeParser(reporter, base, out,
			filter);
	IReporter &mergeReporter = IReporter::create(mergeParser, mergeParser,
			basicFilter);
	if (!conf.keyAsInt("cobertura-only"))
	{
		(void) mkdir(fmt("%s/kcov-merged", base.c_str()).c_str(), 0755);
	}

	// Register writers
	if (runningMode != IConfiguration::MODE_COLLECT_ONLY)
	{
		IWriter &htmlWriter = createHtmlWriter(*parser, reporter, base, out,
				conf.keyAsString("binary-name"));
		IWriter &jsonWriter = createJsonWriter(*parser, reporter,
				out + "/coverage.json");
		IWriter &coberturaWriter = createCoberturaWriter(*parser, reporter,
				out);
		IWriter &sonarqubeWriter = createSonarqubeWriter(*parser, reporter,
				out + "/sonarqube.xml");
		IWriter &codecovWriter = createCodecovWriter(*parser, reporter,
				out + "/codecov.json");

		IWriter &mergeHtmlWriter = createHtmlWriter(mergeParser, mergeReporter,
				base, base + "/kcov-merged", conf.keyAsString("merged-name"),
				false);
		IWriter &mergeJsonWriter = createJsonWriter(mergeParser, mergeReporter,
				base + "kcov-merged/coverage.json");
		IWriter &mergeCoberturaWriter = createCoberturaWriter(mergeParser,
				mergeReporter, base + "kcov-merged");
		IWriter &mergeSonarqubeWriter = createSonarqubeWriter(mergeParser,
				mergeReporter, base + "kcov-merged/sonarqube.xml");
		IWriter &mergeCodecovWriter = createCodecovWriter(mergeParser,
				mergeReporter, base + "kcov-merged/codecov.json");

		if (!conf.keyAsInt("cobertura-only"))
		{
			// Multiple binaries? Register the merged mode stuff
			if (countMetadata() > 0)
			{
				output.registerWriter(mergeHtmlWriter);
				output.registerWriter(mergeJsonWriter);
				output.registerWriter(mergeCoberturaWriter);
				output.registerWriter(mergeSonarqubeWriter);
				output.registerWriter(
						createCoverallsWriter(mergeParser, mergeReporter));
				output.registerWriter(mergeCodecovWriter);
			}
			else
			{
				output.registerWriter(createCoverallsWriter(*parser, reporter));
			}

			output.registerWriter(coberturaWriter);
			output.registerWriter(htmlWriter);
			output.registerWriter(jsonWriter);
			output.registerWriter(sonarqubeWriter);
			output.registerWriter(codecovWriter);
		}
		else
		{
			// Nothing else, if only cobertura is requested
			output.registerWriter(coberturaWriter);
		}
	}

	if (!conf.keyAsInt("cobertura-only"))
	{
		output.registerWriter(mergeParser);
	}

	g_engine = engine;
	g_output = &output;
	g_reporter = &reporter;
	g_collector = &collector;
	g_solibHandler = &solibHandler;
	g_filter = &filter;
	g_basicFilter = &basicFilter;
	signal(SIGINT, ctrlc);
	signal(SIGTERM, ctrlc);

	if (conf.keyAsInt("daemonize-on-first-process-exit"))
		daemonize();

	parser->setupParser(&filter);
	output.start();
	solibHandler.startup();

	int ret = 0;

	if (runningMode != IConfiguration::MODE_REPORT_ONLY)
	{
		ret = collector.run(file);
	}
	else
	{
		parser->parse();
	}

	do_cleanup();

	return ret;
}

static int runSystemModeRecordFile(const std::string &dir,
		const std::string &file, mode_t dirMode, mode_t mode)
{
	pid_t child = fork();
	if (child < 0)
	{
		panic("Fork failed?\n");
	}
	else if (child == 0)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		std::string rootDir = conf.keyAsString("binary-path")
				+ conf.keyAsString("binary-name");

		if (dir.size() < rootDir.size())
		{
			error("kcov: Something strange with the directories: %s vs %s\n",
					dir.c_str(), rootDir.c_str());
			return -1;
		}

		std::string dstDir = dir_concat(conf.keyAsString("out-directory"),
				dir.substr(rootDir.size()));
		std::string dstFile = dir_concat(conf.keyAsString("out-directory"),
				file.substr(rootDir.size()));

		conf.setKey("system-mode-write-file", dstFile);
		conf.setKey("system-mode-write-file-mode", mode & 07777);
		conf.setKey("binary-name", file);
		conf.setKey("binary-path", ""); // file uses an absolute path

		(void) mkdir(dstDir.c_str(), dirMode & 07777);

		runKcov(IConfiguration::MODE_COLLECT_ONLY);
		exit(0);
	}
	else
	{
		// Parent
		int status;

		::waitpid(child, &status, 0);
	}

	return 0;
}

static int runSystemModeRecordDirectory(const std::string &base, mode_t mode)
{
	DIR *dir = ::opendir(base.c_str());
	if (!dir)
	{
		error("Can't open directory %s\n", base.c_str());
		return -1;
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
			runSystemModeRecordDirectory(cur, st.st_mode);
		}
		else if (S_ISREG(st.st_mode)
				&& (st.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)))
		{
			// Executable file?
			runSystemModeRecordFile(base, cur, mode, st.st_mode);
		}
	}
	::closedir(dir);

	return 0;
}

static int runSystemModeRecord()
{
	IConfiguration &conf = IConfiguration::getInstance();

	std::string base = conf.keyAsString("binary-path")
			+ conf.keyAsString("binary-name");
	std::string out = conf.keyAsString("out-directory");
	std::string lib = out + "/lib";

	umask(~0777);
	(void) mkdir(out.c_str(), 0755);
	(void) mkdir(lib.c_str(), 0755);

	std::string library = fmt("%s/libkcov_system.so", lib.c_str());

	if (write_file(kcov_system_library_data.data(),
			kcov_system_library_data.size(), "%s", library.c_str()) < 0)
	{
		error("Can't write binary library at %s", library.c_str());

		return -1;
	}
	if (chmod(library.c_str(), 0755) < 0)
	{
		error("Can't chmod??\n");
	}

	return runSystemModeRecordDirectory(base, 0755);
}

std::vector<std::string> optionsStringToConfigurationVector(
		const std::string &in)
{
	std::vector<std::string> parts = split_string(in, " ");
	if (parts.size() != 6)
	{
		warning("Options vector is not correct: '%s' splits into %zu\n",
				in.c_str(), parts.size());
		return std::vector<std::string>();
	}

	return parts;
}

static int runSystemModeReportFile(const std::string &file)
{
	kcov_system_mode::system_mode_memory *data = kcov_system_mode::diskToMemory(
			file);

	if (!data)
	{
		return -1;
	}

	pid_t child = fork();
	if (child < 0)
	{
		panic("Fork failed?\n");
	}
	else if (child == 0)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		std::pair<std::string, std::string> both = split_path(data->filename); // absolute path

		conf.setKey("system-mode-read-results-file", file);
		conf.setKey("binary-name", both.second);
		conf.setKey("binary-path", both.first);
		conf.setKey("target-directory",
				conf.keyAsString("out-directory") + "/" + both.second);

		std::vector<std::string> options = optionsStringToConfigurationVector(
				data->options);
		if (options.size() == 6)
		{
			conf.setKey("include-pattern", options[0]);
			conf.setKey("exclude-pattern", options[1]);
			conf.setKey("include-path", options[2]);
			conf.setKey("exclude-path", options[3]);
			conf.setKey("exclude-line", options[4]);
			conf.setKey("exclude-region", options[5]);
		}

		runKcov(IConfiguration::MODE_COLLECT_AND_REPORT);
		exit(0);
	}
	else
	{
		// Parent
		int status;

		::waitpid(child, &status, 0);
	}

	delete data;

	return 0;
}

static int runSystemModeReportDirectory(const std::string &base)
{
	DIR *dir = ::opendir(base.c_str());
	if (!dir)
	{
		error("Can't open directory %s\n", base.c_str());
		return -1;
	}

	// Loop through the directory structure
	struct dirent *de;
	for (de = ::readdir(dir); de; de = ::readdir(dir))
	{
		std::string cur = base + "/" + de->d_name;
		struct stat st;

		if (lstat(cur.c_str(), &st) < 0)
			continue;

		// Executable file?
		if (S_ISREG(st.st_mode))
		{
			runSystemModeReportFile(cur);
		}
	}
	::closedir(dir);

	return 0;
}

static int runSystemModeReport()
{
	IConfiguration &conf = IConfiguration::getInstance();

	std::string base = conf.keyAsString("binary-path")
			+ conf.keyAsString("binary-name");

	return runSystemModeReportDirectory(base);
}

int main(int argc, const char *argv[])
{
	IConfiguration &conf = IConfiguration::getInstance();

	if (!conf.parse(argc, argv))
		return 1;

	IConfiguration::RunMode_t runningMode =
			(IConfiguration::RunMode_t) conf.keyAsInt("running-mode");

	if (runningMode == IConfiguration::MODE_MERGE_ONLY)
		return runMergeMode();

	if (runningMode == IConfiguration::MODE_SYSTEM_RECORD)
		return runSystemModeRecord();

	if (runningMode == IConfiguration::MODE_SYSTEM_REPORT)
		return runSystemModeReport();

	return runKcov(runningMode);
}
