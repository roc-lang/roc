namespace std
{
class type_info;
}

#include <reporter.hh>
#include <file-parser.hh>
#include <configuration.hh>
#include <writer.hh>
#include <utils.hh>

#include <string>
#include <list>
#include <unordered_map>
#include <iostream>
#include <fstream>

#include "writer-base.hh"
#include "nocover.hh"

using namespace kcov;

class JsonWriter : public WriterBase
{
public:
	JsonWriter(IFileParser &parser, IReporter &reporter, const std::string &outFile) :
			WriterBase(parser, reporter), m_outFile(outFile)
	{
	}

	void onStartup()
	{
	}

	void onStop()
	{
	}

	void write()
	{
		std::ofstream out(m_outFile);

		// Output directory not writable?
		if (!out.is_open())
			return;

		out << "{\n";
		out << "  \"files\": [\n";

		IConfiguration &conf = IConfiguration::getInstance();
		unsigned int nTotalExecutedLines = 0;
		unsigned int nTotalCodeLines = 0;
		double percentCovered = 0.0;
		bool firstFile = true;

		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *file = it->second;
			unsigned int nExecutedLines = 0;
			unsigned int nCodeLines = 0;

			for (unsigned int n = 1; n < file->m_lastLineNr; n++)
			{
				IReporter::LineExecutionCount cnt = m_reporter.getLineExecutionCount(file->m_name, n);

				const std::string& line_str = file->m_lineMap[n];
				const std::string& file_name = it->first;
				bool should_cover = shouldCover(line_str, file_name);

				if (m_reporter.lineIsCode(file->m_name, n))
				{
					if (should_cover) {
						nExecutedLines += !!cnt.m_hits;
						nCodeLines++;
						nTotalExecutedLines += !!cnt.m_hits;
						nTotalCodeLines++;
					} else {
						// skip this from counting
					}
				}
			}
			if (nCodeLines > 0)
				percentCovered = static_cast<double>(nExecutedLines) / nCodeLines * 100;

			if (firstFile)
				firstFile = false;
			else
				out << ",\n";

			out << fmt("    {"
					"\"file\": \"%s\", "
					"\"percent_covered\": \"%.2f\", "
					"\"covered_lines\": \"%d\", "
					"\"total_lines\": \"%d\""
					"}", file->m_name.c_str(), percentCovered, nExecutedLines, nCodeLines);
		}

		percentCovered = 0;
		if (nTotalCodeLines > 0)
			percentCovered = static_cast<double>(nTotalExecutedLines) / nTotalCodeLines * 100;

		out
				<< fmt("\n"
						"  ],\n"
						"  \"percent_covered\": \"%.2f\",\n"
						"  \"covered_lines\": %d,\n"
						"  \"total_lines\": %d,\n"
						"  \"percent_low\": %d,\n"
						"  \"percent_high\": %d,\n"
						"  \"command\": \"%s\",\n"
						"  \"date\": \"%s\"\n"
						"}\n", percentCovered, nTotalExecutedLines, nTotalCodeLines, conf.keyAsInt("low-limit"),
						conf.keyAsInt("high-limit"), escape_json(conf.keyAsString("command-name")).c_str(),
						getDateNow().c_str());

	}

private:
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

	std::string m_outFile;
};

namespace kcov
{
	IWriter &createJsonWriter(IFileParser &parser, IReporter &reporter, const std::string &outFile)
	{
		return *new JsonWriter(parser, reporter, outFile);
	}
}
