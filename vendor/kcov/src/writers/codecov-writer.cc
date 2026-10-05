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
#include <vector>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <sstream>

#include "writer-base.hh"
#include "nocover.hh"

namespace kcov
{

namespace
{

// Outputs a coverage file that conforms to the Codecov Custom Coverage Format
// as described here: https://docs.codecov.com/docs/codecov-custom-coverage-format
//
// Noteably, while this file format can be consumed by Codecov, it is most
// useful for interoperating with other services that expect this format, as
// Codecov intentionally can consume multiple formats.
class CodecovWriter : public WriterBase
{
public:
	CodecovWriter(IFileParser &parser, IReporter &reporter, const std::string &outFile) :
			WriterBase(parser, reporter), m_outFile(outFile), m_maxPossibleHits(parser.maxPossibleHits())
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
		std::stringstream out;

		setupCommonPaths();

		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *file = it->second;

			// Fixup file->m_codeLines etc
			sumOne(file);
		}
		out << getHeader();

		bool first_time = true;
		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			if (!first_time) {
				out << ",\n";
			}
			File *file = it->second;
			out << writeOne(file);
			first_time = false;
		}
		out << "\n";

		out << getFooter();

		std::ofstream cur(m_outFile);
		cur << out.str();
	}

private:
	void sumOne(File *file)
	{
		unsigned int nExecutedLines = 0;
		unsigned int nCodeLines = 0;

		for (unsigned int n = 1; n < file->m_lastLineNr; n++)
		{
			if (!m_reporter.lineIsCode(file->m_name, n))
				continue;

			IReporter::LineExecutionCount cnt = m_reporter.getLineExecutionCount(file->m_name, n);

			nExecutedLines += !!cnt.m_hits;
			nCodeLines++;

			// Update the execution count
			file->m_executedLines = nExecutedLines;
			file->m_codeLines = nCodeLines;
		}
	}

	std::string writeOne(File *file)
	{
		unsigned int nExecutedLines = 0;
		unsigned int nCodeLines = 0;
		IConfiguration& conf = IConfiguration::getInstance();

		// Compute filename, stripping paths
		std::string filename = file->m_name;

		if (conf.keyAsInt("codecov-full-paths") == 0)
		{
			std::string stripPath = conf.keyAsString("strip-path");
			if (stripPath.size() == 0)
			{
				stripPath = m_commonPath;
			}
			size_t pos = filename.find(stripPath);
			if (pos != std::string::npos && filename.size() > stripPath.size())
			{
				filename = filename.substr(stripPath.size() + 1);
			}
		}

		// Produce each line score.
		std::vector<std::string> lineEntries;
		for (unsigned int n = 1; n < file->m_lastLineNr; n++)
		{
			if (m_reporter.lineIsCode(file->m_name, n))
			{
				IReporter::LineExecutionCount cnt = m_reporter.getLineExecutionCount(file->m_name, n);
				std::string hitScore = "0";

				std::string& line_str = file->m_lineMap.at(n);
				std::string& file_name = file->m_fileName;
				if (!shouldCover(line_str, file_name)) {
					continue;
				}

				if (m_maxPossibleHits == IFileParser::HITS_UNLIMITED || m_maxPossibleHits == IFileParser::HITS_SINGLE)
				{
					if (cnt.m_hits) {
						hitScore = fmt("%u", cnt.m_hits);
					}
				}
				else
				{ // One or multiple for a line
					hitScore = fmt("\"%u/%u\"", cnt.m_hits, cnt.m_possibleHits);
				}
				lineEntries.push_back(fmt("      \"%u\": %s", n, hitScore.c_str()));

				nExecutedLines += !!cnt.m_hits;
				nCodeLines++;
			} else {
				// We could emit '"linenum": null,' though that seems wasteful in storage.
			}

			// Update the execution count.
			file->m_executedLines = nExecutedLines;
			file->m_codeLines = nCodeLines;
		}

		std::string linesBlock;
		for (std::vector<std::string>::const_iterator lineEntry = lineEntries.begin(); lineEntry != lineEntries.end(); ++lineEntry) {
			if (!linesBlock.empty()) {
				linesBlock += ",\n";
			}
			linesBlock += *lineEntry;
		}
		linesBlock += "\n";


		std::string out =
			"    \"" + filename + "\": {\n" +
			linesBlock +
			"    }";

		return out;
	}

	std::string getHeader()
	{
		return "{\n"
		       "  \"coverage\": {\n";
	}

	const std::string getFooter()
	{
		return "  }\n"
		       "}\n";
	}

	std::string m_outFile;
	IFileParser::PossibleHits m_maxPossibleHits;
};

}  // namespace anonymous

IWriter &createCodecovWriter(IFileParser &parser, IReporter &reporter, const std::string &outFile)
{
	return *new CodecovWriter(parser, reporter, outFile);
}
}  // namespace kcov
