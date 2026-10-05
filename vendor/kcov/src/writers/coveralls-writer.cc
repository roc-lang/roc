/*
 * Writer for the "coveralls.io" web service.
 */
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

#include <curl/curl.h>
#include <string.h>

#include "writer-base.hh"

using namespace kcov;

class CurlConnectionHandler
{
public:
	CurlConnectionHandler() :
		m_headerlist(NULL)
	{
		static const char buf[] = "Expect:";

		// Workaround proxy problems
		m_headerlist = curl_slist_append(m_headerlist, buf);

		curl_global_init(CURL_GLOBAL_ALL);

		m_curl = curl_easy_init();

		// Setup curl to read from memory
		curl_easy_setopt(m_curl, CURLOPT_URL, "https://coveralls.io/api/v1/jobs");
		curl_easy_setopt(m_curl, CURLOPT_WRITEFUNCTION, curlWriteFuncStatic);
		curl_easy_setopt(m_curl, CURLOPT_WRITEDATA, (void *)this);
		curl_easy_setopt(m_curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
		curl_easy_setopt(m_curl, CURLOPT_HTTPHEADER, m_headerlist); // Proxy issues
	}

	~CurlConnectionHandler()
	{
		curl_slist_free_all(m_headerlist);
		curl_easy_cleanup(m_curl);
		curl_global_cleanup();
	}


	// Send fileName and data to the remote server
	bool talk(const std::string &fileName)
	{
		m_writtenData = "";

		struct curl_httppost *formpost = NULL;
		struct curl_httppost *lastptr = NULL;
		CURLcode res;

		curl_formadd(&formpost,
				&lastptr,
				CURLFORM_COPYNAME, "json_file",
				CURLFORM_FILE, fileName.c_str(),
				CURLFORM_END);

		curl_easy_setopt(m_curl, CURLOPT_HTTPPOST, formpost);

		res = curl_easy_perform(m_curl);

		curl_formfree(formpost);

		if (res != CURLE_OK)
			return false;

		if (m_writtenData.find("Job #") == std::string::npos)
		{
			warning("coveralls write failed: %s\n", m_writtenData.c_str());
			return false;
		}

		return true;
	}

private:
	size_t curlWritefunc(void *ptr, size_t size, size_t nmemb)
	{
		const char *p = (char *)ptr;

		m_writtenData.append(p, size * nmemb);

		return size * nmemb;
	}

	static size_t curlWriteFuncStatic(void *ptr, size_t size, size_t nmemb, void *priv)
	{
		if (!priv)
			return 0;

		return ((CurlConnectionHandler *)priv)->curlWritefunc(ptr, size, nmemb);
	}

	CURL *m_curl;
	std::string m_writtenData;
	struct curl_slist *m_headerlist;
};

static CurlConnectionHandler *g_curl;


class CoverallsWriter : public WriterBase
{
public:
	CoverallsWriter(IFileParser &parser, IReporter &reporter) :
		WriterBase(parser, reporter),
		m_doWrite(false)
	{
	}

	void onStartup()
	{
	}

	void onStop()
	{
		m_doWrite = true;
	}

	void write()
	{
		// Write once at the end only
		if (!m_doWrite)
			return;

		IConfiguration &conf = IConfiguration::getInstance();
		const std::string &id = conf.keyAsString("coveralls-id");

		// No token? Skip output then
		if (id == "")
			return;

		std::string outFile = conf.keyAsString("target-directory") + "/coveralls.out";

		// Output file with coveralls json data
		std::ofstream out(outFile);

		// Output directory not writable?
		if (!out.is_open())
			return;

		out << "{\n";
		if (isRepoToken(id))
		{
			out << " \"repo_token\": \"" + id + "\",\n";
		}
		else
		{
			out << " \"service_name\": \"" + conf.keyAsString("coveralls-service-name") + "\",\n";
			out << " \"service_job_id\": \"" + id + "\",\n";
		}
		out << " \"source_files\": [\n";
		setupCommonPaths();
		
		std::string strip_path = conf.keyAsString("strip-path");
		if (strip_path.size() == 0)
		{
			setupCommonPaths();
			strip_path = m_commonPath + "/";
		}

		unsigned int filesLeft = m_files.size();
		for (FileMap_t::const_iterator it = m_files.begin();
				it != m_files.end();
				++it)
		{
			File *file = it->second;
			std::string fileName;

			// Strip away the specified path (unless this is the only file)
			if (file->m_name.compare(0, strip_path.length(), strip_path) == 0)
				fileName = file->m_name.substr(strip_path.size());
			else
				fileName = file->m_fileName;

			out << "  {\n";
			out << "   \"name\": \"" + escape_json(fileName) + "\",\n";
			// Use hash as source file
			out << fmt("   \"source_digest\": \"0x%08lx\",\n", (unsigned long)file->m_crc);
			out << "   \"coverage\": [";

			// And coverage
			for (unsigned int n = 1; n < file->m_lastLineNr; n++)
			{
				if (!m_reporter.lineIsCode(file->m_name, n))
				{
					out << "null";
				}
				else
				{
					IReporter::LineExecutionCount cnt =
							m_reporter.getLineExecutionCount(file->m_name, n);

					out << cnt.m_hits;
				}

				if (n != file->m_lastLineNr - 1)
					out << ",";
			}
			out << "]\n";

			// Add comma or not on the last run
			filesLeft--;
			if (filesLeft > 0)
				out << "  },\n";
			else
				out << "  }\n";
		}
		out << " ]\n";
		out << "}\n";

		out.close();

		// Create singleton
		if (!g_curl)
			g_curl = new CurlConnectionHandler();

		if (id != "dry-run")
			g_curl->talk(outFile);
	}

private:
	bool isRepoToken(const std::string &str)
	{
		return str.size() >= 32;
	}

	bool m_doWrite;
};

namespace kcov
{
	IWriter &createCoverallsWriter(IFileParser &parser, IReporter &reporter)
	{
		return *new CoverallsWriter(parser, reporter);
	}
}
