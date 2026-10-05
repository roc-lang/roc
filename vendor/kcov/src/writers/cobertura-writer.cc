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

using namespace kcov;

class CoberturaWriter : public WriterBase
{
public:
	CoberturaWriter(IFileParser &parser, IReporter &reporter, const std::string &outDir) :
			WriterBase(parser, reporter), m_maxPossibleHits(parser.maxPossibleHits())
	{
		if (!IConfiguration::getInstance().keyAsInt("cobertura-only"))
		{
			// cobertura-only is used for vscode, normally
			m_outFiles.push_back(outDir + "/cobertura.xml");
		}
		m_outFiles.push_back(outDir + "/cov.xml"); // For vscode coverage gutters
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

		unsigned int nTotalExecutedLines = 0;
		unsigned int nTotalCodeLines = 0;

		setupCommonPaths();

		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *file = it->second;

			// Fixup file->m_codeLines etc
			sumOne(file);

			nTotalCodeLines += file->m_codeLines;
			nTotalExecutedLines += file->m_executedLines;
		}
		out << getHeader(nTotalCodeLines, nTotalExecutedLines);

		for (FileMap_t::const_iterator it = m_files.begin(); it != m_files.end(); ++it)
		{
			File *file = it->second;

			out << writeOne(file);
		}

		out << getFooter();
		for (std::vector<std::string>::iterator it = m_outFiles.begin();
			it != m_outFiles.end();
			++it)
		{
			std::ofstream cur(*it);
			cur << out.str();
		}
	}

private:
	const std::string mangleFileName(const std::string &name)
	{
		std::string out = name;

		for (size_t pos = 0; pos < name.size(); pos++)
		{
			if (out[pos] == '.' || out[pos] == '-')
				out[pos] = '_';
		}

		return out;
	}

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

			unsigned int hits = cnt.m_hits;

			if (hits && m_maxPossibleHits == IFileParser::HITS_SINGLE)
				hits = 1;

			// Update the execution count
			file->m_executedLines = nExecutedLines;
			file->m_codeLines = nCodeLines;
		}
	}

	std::string writeOne(File *file)
	{
		static uint32_t counter;
		std::string out;

		std::string mangledName = mangleFileName(file->m_fileName) + "__" + std::to_string(counter++);

		for (unsigned int n = 1; n < file->m_lastLineNr; n++)
		{
			if (!m_reporter.lineIsCode(file->m_name, n))
				continue;

			IReporter::LineExecutionCount cnt = m_reporter.getLineExecutionCount(file->m_name, n);

			unsigned int hits = cnt.m_hits;

			if (hits && m_maxPossibleHits == IFileParser::HITS_SINGLE)
				hits = 1;

			out = out + "						<line number=\"" + fmt("%u", n) + "\" hits=\"" + fmt("%u", hits) + "\"/>\n";
		}

		unsigned nCodeLines = file->m_codeLines;
		if (nCodeLines == 0)
			nCodeLines = 1;

		std::string filename = file->m_name;
		size_t pos = filename.find(m_commonPath);

		if (IConfiguration::getInstance().keyAsInt("cobertura-full-paths") == 0)
		{
			if (pos != std::string::npos && filename.size() > m_commonPath.size())
				filename = filename.substr(m_commonPath.size() + 1);
		}

		out = "				<class name=\"" + mangledName + "\" filename=\"" + filename + "\" branch-rate=\"1.0\" complexity=\"1.0\" line-rate=\""
				+ fmt("%.3f", file->m_executedLines / (float) nCodeLines) + "\">\n" + "					<lines>\n" + out + "					</lines>\n"
						"				</class>\n";

		return out;
	}

	std::string getHeader(unsigned int nCodeLines, unsigned int nExecutedLines)
	{
		time_t t;
		struct tm *tm;
		char date_buf[80];

		t = time(NULL);
		tm = localtime(&t);
		strftime(date_buf, sizeof(date_buf), "%s", tm);

		std::string linesCovered = fmt("%u", nExecutedLines);
		std::string linesValid = fmt("%u", nCodeLines);

		if (nCodeLines == 0)
			nCodeLines = 1;

		std::string lineRate = fmt("%.3f", nExecutedLines / (float) nCodeLines);

		return "<?xml version=\"1.0\" ?>\n"
				"<!DOCTYPE coverage SYSTEM 'http://cobertura.sourceforge.net/xml/coverage-04.dtd'>\n"
				"<coverage line-rate=\"" + lineRate + "\" lines-covered=\"" + linesCovered + "\" lines-valid=\"" + linesValid + "\" branch-rate=\"1.0\" branches-covered=\"1.0\" branches-rate=\"1.0\" complexity=\"1.0\" version=\"1.9\" timestamp=\"" + std::string(date_buf) + "\">\n"
				"	<sources>\n"
				"		<source>" + m_commonPath + "/</source>\n"
				"	</sources>\n"
				"	<packages>\n"
				"		<package name=\"" + mangleFileName(IConfiguration::getInstance().keyAsString("command-name"))
				+ "\" line-rate=\"" + lineRate + "\" lines-covered=\"" + linesCovered + "\" lines-valid=\"" + linesValid + "\" branch-rate=\"1.0\" complexity=\"1.0\">\n"
						"			<classes>\n";
	}

	const std::string getFooter()
	{
		return "			</classes>\n"
				"		</package>\n"
				"	</packages>\n"
				"</coverage>\n";
	}

	std::vector<std::string> m_outFiles;
	IFileParser::PossibleHits m_maxPossibleHits;
};

namespace kcov
{
	IWriter &createCoberturaWriter(IFileParser &parser, IReporter &reporter, const std::string &outFile)
	{
		return *new CoberturaWriter(parser, reporter, outFile);
	}
}
