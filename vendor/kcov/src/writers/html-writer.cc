#include <reporter.hh>
#include <file-parser.hh>
#include <configuration.hh>
#include <writer.hh>
#include <utils.hh>
#include <generated-data-base.hh>

#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include <string>
#include <list>
#include <vector>
#include <unordered_map>
#include <fstream>

#include "writer-base.hh"
#include "nocover.hh"

using namespace kcov;

// Generated
extern GeneratedData css_text_data;
extern GeneratedData icon_amber_data;
extern GeneratedData icon_glass_data;
extern GeneratedData source_file_text_data;
extern GeneratedData index_text_data;
extern GeneratedData handlebars_text_data;
extern GeneratedData kcov_text_data;
extern GeneratedData jquery_text_data;
extern GeneratedData tablesorter_text_data;
extern GeneratedData tablesorter_widgets_text_data;
extern GeneratedData tablesorter_theme_text_data;

class HtmlWriter : public WriterBase
{
public:
	HtmlWriter(IFileParser &parser, IReporter &reporter, const std::string &indexDirectory, const std::string &outDirectory,
			const std::string &name, bool includeInTotals) :
			WriterBase(parser, reporter), m_outDirectory(outDirectory + "/"), m_indexDirectory(indexDirectory + "/"), m_summaryDbFileName(
					outDirectory + "/summary.db"), m_name(name), m_includeInTotals(includeInTotals), m_maxPossibleHits(
					parser.maxPossibleHits())
	{
	}

	void onStop()
	{
	}

private:

	void writeOne(File *file)
	{
		std::string jsonOutName = m_outDirectory + "/" + file->m_jsonOutFileName;
		std::string htmlOutName = m_outDirectory + "/" + file->m_outFileName;
		unsigned int nExecutedLines = 0;
		unsigned int nCodeLines = 0;

		// Out-file for JSON data
		std::ofstream outJson(jsonOutName);
		// ... and HTML data
		std::ofstream outHtml(htmlOutName);

		outJson << "var data = {lines:[\n";

		// Produce each line in the file
		for (unsigned int n = 1; n < file->m_lastLineNr; n++)
		{
			const std::string &line = file->m_lineMap[n];

			outJson << fmt("{\"lineNum\":\"%5u\","
					"\"line\":\"", n);
			outJson << escape_json(line) << "\"";

			std::string& line_str = file->m_lineMap[n];
			std::string& file_name = file->m_name;
			bool should_cover = shouldCover(line_str, file_name);
			const std::string& no_cover_class = "lineNoCov";

			if (m_reporter.lineIsCode(file->m_name, n))
			{
				IReporter::LineExecutionCount cnt = m_reporter.getLineExecutionCount(file->m_name, n);
				std::string lineClass = "lineNoCov";

				if (m_maxPossibleHits == IFileParser::HITS_UNLIMITED || m_maxPossibleHits == IFileParser::HITS_SINGLE)
				{
					if (cnt.m_hits)
						lineClass = "lineCov";
				}
				else
				{ // One or multiple for a line
					if (cnt.m_hits == cnt.m_possibleHits)
						lineClass = "lineCov";
					else if (cnt.m_hits)
						lineClass = "linePartCov";
				}

				if (lineClass == no_cover_class && !should_cover) {
					// should not cover, so skip, but if there are some hits (when lineClass is not no cov), go else for normal marking
				} else {
					outJson << fmt(",\"class\":\"%s\","
							"\"hits\":\"%u\",", lineClass.c_str(), cnt.m_hits);
					// Don't report order for zeroes
					if (cnt.m_order)
						outJson << fmt("\"order\":\"%llu\",", (unsigned long long) cnt.m_order);

					if (m_maxPossibleHits != IFileParser::HITS_SINGLE)
						outJson << fmt("\"possible_hits\":\"%u\",", cnt.m_possibleHits);

					nExecutedLines += !!cnt.m_hits;
					nCodeLines++;
				}
			}
			outJson << "},\n";

			// Update the execution count
			file->m_executedLines = nExecutedLines;
			file->m_codeLines = nCodeLines;
		}

		// Add the header
		outJson << "]};\n";
		outJson << getHeader(nCodeLines, nExecutedLines);
		outJson << "var merged_data = [];\n";

		// Produce HTML out-file
		outHtml << fmt("<script type=\"text/javascript\" src=\"%s\"></script>\n", file->m_jsonOutFileName.c_str());
		outHtml.write((const char *) source_file_text_data.data(), source_file_text_data.size());
	}

	void writeIndex()
	{
		IConfiguration &conf = IConfiguration::getInstance();
		unsigned int nTotalExecutedLines = 0;
		unsigned int nTotalCodeLines = 0;

		// Out-file for JSON data
		std::ofstream outJson(m_outDirectory + "index.js");
		outJson << "var data = {files:[\n"; // Not really json, but anyway

		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *file = it->second;
			unsigned int nExecutedLines = file->m_executedLines;
			unsigned int nCodeLines = file->m_codeLines;

			nTotalCodeLines += nCodeLines;
			nTotalExecutedLines += nExecutedLines;

			std::string listName = file->m_name;

			size_t pos = listName.find(m_commonPath);
			unsigned int stripLevel = IConfiguration::getInstance().keyAsInt("path-strip-level");

			if (pos != std::string::npos && m_commonPath.size() != 0 && stripLevel != ~0U)
			{
				std::string pathToRemove = m_commonPath;

				for (unsigned int i = 0; i < stripLevel; i++)
				{
					size_t slashPos = pathToRemove.rfind("/");

					if (slashPos == std::string::npos)
						break;
					pathToRemove = pathToRemove.substr(0, slashPos);
				}

				std::string prefix = "[...]";

				if (pathToRemove == "")
					prefix = "";
				listName = prefix + listName.substr(pathToRemove.size());
			}

			outJson << getIndexHeader(file->m_outFileName, file->m_fileName, listName, nCodeLines, nExecutedLines);
		}

		// Add the header
		outJson
				<< fmt("]};\n"
						"var percent_low = %d;"
						"var percent_high = %d;"
						"\n"
						"var header = {"
						" \"command\" : \"%s\","
						" \"date\" : \"%s\","
						" \"instrumented\" : %d,"
						" \"covered\" : %d,"
						"};"
						"\n", conf.keyAsInt("low-limit"), conf.keyAsInt("high-limit"),
						escape_json(conf.keyAsString("command-name")).c_str(), getDateNow().c_str(), nTotalCodeLines,
						nTotalExecutedLines) + "var merged_data = [];\n";

		// Produce HTML outfile
		std::ofstream outHtml(m_outDirectory + "index.html");
		outHtml.write((const char *) index_text_data.data(), index_text_data.size());

		// Produce a summary
		IReporter::ExecutionSummary summary = m_reporter.getExecutionSummary();
		summary.m_includeInTotals = m_includeInTotals;
		size_t sz;

		void *data = marshalSummary(summary, m_name, &sz);

		if (data)
			write_file(data, sz, "%s", m_summaryDbFileName.c_str());

		free(data);
	}

	void writeGlobalIndex()
	{
		DIR *dir;
		struct dirent *de;
		std::string idx = m_indexDirectory.c_str();
		std::string s;
		unsigned int nTotalExecutedLines = 0;
		unsigned int nTotalCodeLines = 0;
		IConfiguration &conf = IConfiguration::getInstance();

		dir = opendir(idx.c_str());
		panic_if(!dir, "Can't open directory %s\n", idx.c_str());

		std::ofstream outJson(m_indexDirectory + "index.js");

		outJson << "var data = {files:[\n";
		std::string merged;

		for (de = readdir(dir); de; de = readdir(dir))
		{
			std::string curDir = idx + de->d_name;
			std::string cur = curDir + "/summary.db";
			struct stat st;

			if (lstat(curDir.c_str(), &st) < 0)
			{
				continue;
			}

			if (S_ISLNK(st.st_mode))
			{
				// Skip human friendly names
				continue;
			}

			if (!file_exists(cur))
				continue;

			size_t sz;
			void *data = read_file(&sz, "%s", cur.c_str());

			if (!data)
				continue;

			IReporter::ExecutionSummary summary;
			std::string name;
			bool res = unMarshalSummary(data, sz, summary, name);
			free(data);

			if (!res)
				continue;

			// since we may ignored some from previous individual writeOne functions for each file, here use the sum from
			// files instead of summary
			int inFilesTotalCodeLines = 0;
			int inFilesTotalExecutedLines = 0;
			for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
			{
				inFilesTotalCodeLines += it->second->m_codeLines;
				inFilesTotalExecutedLines += it->second->m_executedLines;
			}

			// Skip entries (merged ones) that shouldn't be included in the totals
			if (summary.m_includeInTotals)
			{
					nTotalCodeLines += inFilesTotalCodeLines;
					nTotalExecutedLines += inFilesTotalExecutedLines;
					// nTotalCodeLines += summary.m_lines;
					// nTotalExecutedLines += summary.m_executedLines;
			}

			std::string datum = getIndexHeader(fmt("%s/index.html", de->d_name), name, name, inFilesTotalCodeLines,
					inFilesTotalExecutedLines);
			// std::string datum = getIndexHeader(fmt("%s/index.html", de->d_name), name, name, summary.m_lines,
			// summary.m_executedLines);

			if (name == conf.keyAsString("merged-name"))
				merged += datum;
			else
				outJson << datum;
		}

		// Add the header
		outJson << "], merged_files:[" + merged + "]};\n" + getHeader(nTotalCodeLines, nTotalExecutedLines);

		// Produce HTML outfile
		std::ofstream outHtml(m_indexDirectory + "index.html");
		outHtml.write((const char *) index_text_data.data(), index_text_data.size());

		closedir(dir);
	}

	void write()
	{
		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
			writeOne(it->second);

		setupCommonPaths();

		writeIndex();

		if (m_includeInTotals)
			writeGlobalIndex();
	}

	std::string getHeader(unsigned int lines, unsigned int executedLines)
	{
		IConfiguration &conf = IConfiguration::getInstance();

		return fmt("var percent_low = %d;"
				"var percent_high = %d;"
				"\n"
				"var header = {"
				" \"command\" : \"%s\","
				" \"date\" : \"%s\","
				" \"instrumented\" : %d,"
				" \"covered\" : %d,"
				"};"
				"\n", conf.keyAsInt("low-limit"), conf.keyAsInt("high-limit"),
				escape_json(conf.keyAsString("command-name")).c_str(), getDateNow().c_str(), lines, executedLines);
	}

	// Return a header for index-type JSON files
	std::string getIndexHeader(const std::string &linkName, const std::string titleName, const std::string summaryName,
			unsigned int lines, unsigned int executedLines)
	{
		double percent = 0;

		if (lines != 0)
			percent = (executedLines / (double) lines) * 100;

		return fmt("{\"link\":\"%s\","
				"\"title\":\"%s\","
				"\"summary_name\":\"%s\","
				"\"covered_class\":\"%s\","
				"\"covered\":\"%.1f\","
				"\"covered_lines\":\"%d\","
				"\"uncovered_lines\":\"%d\","
				"\"total_lines\" : \"%d\"},\n", escape_url(linkName).c_str(), titleName.c_str(), summaryName.c_str(),
				colorFromPercent(percent).c_str(), percent, executedLines, lines - executedLines, lines);
	}

	std::string colorFromPercent(double percent)
	{
		IConfiguration &conf = IConfiguration::getInstance();

		if (percent >= conf.keyAsInt("high-limit"))
			return "lineCov";
		else if (percent > conf.keyAsInt("low-limit"))
			return "linePartCov";

		return "lineNoCov";
	}

	std::string getDateNow()
	{
		time_t t;
		struct tm *tm;
		char date_buf[128];

		t = time(NULL);
		tm = localtime(&t);
		strftime(date_buf, sizeof(date_buf), "%Y-%m-%d %H:%M:%S", tm);

		return std::string(date_buf);
	}

	void writeHelperFiles(std::string dir)
	{
		IConfiguration &conf = IConfiguration::getInstance();
		GeneratedData css = css_text_data;

		std::string cssFileName = conf.keyAsString("css-file");
		if (cssFileName != "")
		{
			size_t sz;
			uint8_t *p = (uint8_t *) read_file(&sz, "%s", cssFileName.c_str());

			if (p)
				css = GeneratedData(p, sz);
			else
				warning("Can't read CSS file %s\n", cssFileName.c_str());
		}

		(void) mkdir(fmt("%s/data", dir.c_str()).c_str(), 0755);
		(void) mkdir(fmt("%s/data/js", dir.c_str()).c_str(), 0755);
		write_file(icon_amber_data.data(), icon_amber_data.size(), "%s/data/amber.png", dir.c_str());
		write_file(icon_glass_data.data(), icon_glass_data.size(), "%s/data/glass.png", dir.c_str());
		write_file(css.data(), css.size(), "%s/data/bcov.css", dir.c_str());
		write_file(handlebars_text_data.data(), handlebars_text_data.size(), "%s/data/js/handlebars.js", dir.c_str());
		write_file(kcov_text_data.data(), kcov_text_data.size(), "%s/data/js/kcov.js", dir.c_str());
		write_file(jquery_text_data.data(), jquery_text_data.size(), "%s/data/js/jquery.min.js", dir.c_str());
		write_file(tablesorter_text_data.data(), tablesorter_text_data.size(), "%s/data/js/tablesorter.min.js", dir.c_str());
		write_file(tablesorter_widgets_text_data.data(), tablesorter_widgets_text_data.size(),
				"%s/data/js/jquery.tablesorter.widgets.min.js", dir.c_str());
		write_file(tablesorter_theme_text_data.data(), tablesorter_theme_text_data.size(), "%s/data/tablesorter-theme.css",
				dir.c_str());
	}

	void onStartup()
	{
		writeHelperFiles(m_indexDirectory);
		writeHelperFiles(m_outDirectory);
	}

	std::string m_outDirectory;
	std::string m_indexDirectory;
	std::string m_summaryDbFileName;
	std::string m_name;
	bool m_includeInTotals;
	enum IFileParser::PossibleHits m_maxPossibleHits;
};

namespace kcov
{
	IWriter &createHtmlWriter(IFileParser &parser, IReporter &reporter, const std::string &indexDirectory,
			const std::string &outDirectory, const std::string &name, bool includeInTotals)
	{
		return *new HtmlWriter(parser, reporter, indexDirectory, outDirectory, name, includeInTotals);
	}
}
