#include <filter.hh>
#include <configuration.hh>
#include <utils.hh>

#include <limits.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <regex>

using namespace kcov;

class BasicFilter : public IFilter
{
public:
	BasicFilter()
	{
		m_fileLineHandler = new FileLineHandler();
	}

	~BasicFilter()
	{
		delete m_fileLineHandler;
	}

	// Allow anything
	virtual bool runFilters(const std::string &file)
	{
		return true;
	}

	virtual bool runLineFilters(const std::string &filePath,
			unsigned int lineNr, const std::string &line)
	{
		return m_fileLineHandler->match(filePath, lineNr, line);
	}

	virtual std::string mangleSourcePath(const std::string &path)
	{
		return path;
	}

protected:
	class FileLineHandler
	{
	public:
		FileLineHandler()
		{
			m_lineBeginPatterns.push_back("LCOV_EXCL_START");
			m_lineEndPatterns.push_back("LCOV_EXCL_STOP");

			m_ignoreSingleLinePatterns.push_back("LCOV_EXCL_LINE");
			m_ignoreSingleLinePatterns.push_back("#[should_panic]");

			// Handle command line options
			std::string cmd = IConfiguration::getInstance().keyAsString(
					"exclude-line");

			if (cmd != "")
			{
				std::vector<std::string> cmds = split_string(cmd, ",");

				for (std::vector<std::string>::iterator it = cmds.begin();
						it != cmds.end(); ++it)
					m_ignoreSingleLinePatterns.push_back(*it);
			}

			std::string startStop = IConfiguration::getInstance().keyAsString(
					"exclude-region");

			if (startStop != "")
			{
				std::vector<std::string> cmds = split_string(startStop, ",");

				for (std::vector<std::string>::iterator it = cmds.begin();
						it != cmds.end(); ++it)
				{
					std::vector<std::string> entries = split_string(*it, ":");

					if (entries.size() >= 1)
						m_lineBeginPatterns.push_back(entries[0]);
					if (entries.size() > 1)
						m_lineEndPatterns.push_back(entries[1]);

				}
			}

			m_excludeStart = 0;
		}

		bool match(const std::string &filePath, unsigned int lineNr,
				const std::string &line)
		{
			bool out = true;

			// Source file swapped
			if (m_curFile != filePath)
			{
				m_excludeStart = 0;
				m_curFile = filePath;
			}

			for (std::vector<std::string>::iterator it =
					m_ignoreSingleLinePatterns.begin();
					it != m_ignoreSingleLinePatterns.end(); ++it)
			{
				if (line.find(*it) != std::string::npos)
					out = false;
			}

			for (std::vector<std::string>::iterator it =
					m_lineBeginPatterns.begin();
					it != m_lineBeginPatterns.end(); ++it)
			{
				if (line.find(*it) != std::string::npos)
				{
					m_excludeStart++;
					break;
				}
			}

			// The line including the stop should be covered
			if (m_excludeStart > 0)
				out = false;

			for (std::vector<std::string>::iterator it =
					m_lineEndPatterns.begin(); it != m_lineEndPatterns.end();
					++it)
			{
				if (line.find(*it) != std::string::npos)
				{
					m_excludeStart--;
					break;
				}
			}

			// Ignore multiple stops
			if (m_excludeStart < 0)
				m_excludeStart = 0;

			return out;
		}

	private:
		std::string m_curFile;
		std::vector<std::string> m_ignoreSingleLinePatterns;

		std::vector<std::string> m_lineBeginPatterns;
		std::vector<std::string> m_lineEndPatterns;
		int m_excludeStart;
	};

	FileLineHandler *m_fileLineHandler;
};

class Filter: public BasicFilter
{
public:
	Filter()
	{
		m_patternHandler = new PatternHandler();
		m_pathHandler = new PathHandler();

		m_origRoot = IConfiguration::getInstance().keyAsString(
				"orig-path-prefix");
		m_newRoot = IConfiguration::getInstance().keyAsString(
				"new-path-prefix");
	}

	~Filter()
	{
		delete m_patternHandler;
		delete m_pathHandler;
	}

	// Used by the unit test
	void setup()
	{
		delete m_fileLineHandler;
		delete m_patternHandler;
		delete m_pathHandler;
		m_patternHandler = new PatternHandler();
		m_pathHandler = new PathHandler();
		m_fileLineHandler = new FileLineHandler();
	}

	bool runFilters(const std::string &file)
	{
		bool out = true;

		if (m_pathHandler->isSetup())
			out = m_pathHandler->includeFile(file);

		if (out && m_patternHandler->isSetup())
			return m_patternHandler->includeFile(file);

		return out;
	}

	std::string mangleSourcePath(const std::string &path)
	{
		std::string filename = get_real_path(path);
		if (m_origRoot.length() > 0 && m_newRoot.length() > 0)
		{
			std::regex regexRoot(m_origRoot);
			filename = get_real_path(regex_replace (filename,regexRoot,m_newRoot));
			
		}
		return filename;
	}

private:
	class PatternHandler
	{
	public:
		PatternHandler() :
				m_includePatterns(
						IConfiguration::getInstance().keyAsList(
								"include-pattern")), m_excludePatterns(
						IConfiguration::getInstance().keyAsList(
								"exclude-pattern"))
		{
		}

		bool isSetup()
		{
			return !(m_includePatterns.size() == 0
					&& m_excludePatterns.size() == 0);
		}

		bool includeFile(std::string file)
		{
			if (m_includePatterns.size() == 0 && m_excludePatterns.size() == 0)
				return true;

			bool out = true;

			if (m_includePatterns.size() != 0)
				out = false;

			for (PatternMap_t::const_iterator it = m_includePatterns.begin();
					it != m_includePatterns.end(); ++it)
			{
				const std::string &pattern = *it;

				if (file.find(pattern) != std::string::npos)
					out = true;
			}

			for (PatternMap_t::const_iterator it = m_excludePatterns.begin();
					it != m_excludePatterns.end(); ++it)
			{
				const std::string &pattern = *it;

				if (file.find(pattern) != std::string::npos)
					out = false;
			}

			return out;
		}
	private:
		typedef std::vector<std::string> PatternMap_t;

		const PatternMap_t &m_includePatterns;
		const PatternMap_t &m_excludePatterns;
	};

	class PathHandler
	{
	public:
		PathHandler() :
				m_includePaths(
						IConfiguration::getInstance().keyAsList(
								"include-path")), m_excludePaths(
						IConfiguration::getInstance().keyAsList("exclude-path"))
		{
			for (PathMap_t::iterator it = m_includePaths.begin();
					it != m_includePaths.end(); ++it)
				*it = get_real_path(*it);

			for (PathMap_t::iterator it = m_excludePaths.begin();
					it != m_excludePaths.end(); ++it)
				*it = get_real_path(*it);
		}

		bool isSetup()
		{
			return !(m_includePaths.size() == 0 && m_excludePaths.size() == 0);
		}

		bool includeFile(const std::string &file)
		{
			if (m_includePaths.size() == 0 && m_excludePaths.size() == 0)
				return true;

			bool out = true;

			if (m_includePaths.size() != 0)
				out = false;

			const std::string pathStr = get_real_path(file);

			// If found in --include-path=, accept this path
			for (PathMap_t::const_iterator it = m_includePaths.begin();
					it != m_includePaths.end(); ++it)
			{
				const std::string &pathPattern = *it;
				size_t last = pathPattern.size();

				if (pathStr.find(pathPattern) == 0
						&& (pathStr.size() <= last || pathStr[last] == '/'))
				{
					out = true;
					break;
				}
			}

			// ... unless it's found in --exclude-path=.
			for (PathMap_t::const_iterator it = m_excludePaths.begin();
					it != m_excludePaths.end(); ++it)
			{
				const std::string &pathPattern = *it;
				size_t last = pathPattern.size();

				if (pathStr.find(pathPattern) == 0
						&& (pathStr.size() <= last || pathStr[last] == '/'))
				{
					out = false;
					break;
				}
			}

			return out;
		}
	private:
		typedef std::vector<std::string> PathMap_t;

		PathMap_t m_includePaths;
		PathMap_t m_excludePaths;
	};

	PatternHandler *m_patternHandler;
	PathHandler *m_pathHandler;
	std::string m_origRoot;
	std::string m_newRoot;
};

IFilter &IFilter::create()
{
	return *new Filter();
}

IFilter &IFilter::createBasic()
{
	return *new BasicFilter();
}
