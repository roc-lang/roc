#include <output-handler.hh>
#include <writer.hh>
#include <configuration.hh>
#include <reporter.hh>
#include <collector.hh>
#include <file-parser.hh>
#include <utils.hh>

#include <list>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <dirent.h>

namespace kcov
{
	class OutputHandler :
			public IOutputHandler,
			public ICollector::IEventTickListener
	{
	public:
		OutputHandler(IReporter &reporter, ICollector *collector)
		{
			IConfiguration &conf = IConfiguration::getInstance();

			m_baseDirectory = conf.keyAsString("out-directory");
			m_outDirectory = conf.keyAsString("target-directory") + "/";
			m_summaryDbFileName = m_outDirectory + "/summary.db";
			m_outputInterval = conf.keyAsInt("output-interval");

			m_lastTimestamp = get_ms_timestamp();

			std::string readableName = fmt("%s/%s", m_baseDirectory.c_str(),  conf.keyAsString("binary-name").c_str());

			(void)mkdir(m_baseDirectory.c_str(), 0755);
			(void)mkdir(m_outDirectory.c_str(), 0755);

			if (!conf.keyAsInt("cobertura-only"))
			{
				(void)unlink(readableName.c_str());

				std::string out_path = get_real_path(m_outDirectory);
				if (symlink(out_path.c_str(), readableName.c_str()) < 0)
				{
					kcov_debug(INFO_MSG, "Can't symlink readable name\n");
				}

			}

			if (collector)
				collector->registerEventTickListener(*this);
		}

		~OutputHandler()
		{
			stop();

			// Delete all writers
			for (WriterList_t::iterator it = m_writers.begin();
					it != m_writers.end();
					++it)
			{
				IWriter *cur = *it;

				delete cur;
			}
		}

		const std::string &getBaseDirectory()
		{
			return m_baseDirectory;
		}

		const std::string &getOutDirectory()
		{
			return m_outDirectory;
		}

		void registerWriter(IWriter &writer)
		{
			m_writers.push_back(&writer);
		}

		void start()
		{
			for (WriterList_t::const_iterator it = m_writers.begin();
					it != m_writers.end();
					++it)
				(*it)->onStartup();
		}

		void stop()
		{
			for (WriterList_t::const_iterator it = m_writers.begin();
					it != m_writers.end();
					++it)
				(*it)->onStop();

			// Produce output after stop if anyone yields new data in onStop()
			produce();
		}

		void produce()
		{
			for (WriterList_t::const_iterator it = m_writers.begin();
					it != m_writers.end();
					++it)
				(*it)->write();
		}

		// From ICollector::IEventTickListener
		void onTick()
		{
			if (m_outputInterval == 0)
				return;

			if (get_ms_timestamp() - m_lastTimestamp >= m_outputInterval)
			{
				produce();

				// Take a new timestamp since producing might take a long time
				m_lastTimestamp = get_ms_timestamp();
			}
		}

	private:
		typedef std::vector<IWriter *> WriterList_t;

		std::string m_outDirectory;
		std::string m_baseDirectory;
		std::string m_summaryDbFileName;

		WriterList_t m_writers;

		unsigned int m_outputInterval;
		uint64_t m_lastTimestamp;
	};

	static OutputHandler *instance;
	IOutputHandler &IOutputHandler::create(IReporter &reporter, ICollector *collector)
	{
		if (!instance)
			instance = new OutputHandler(reporter, collector);

		return *instance;
	}

	IOutputHandler &IOutputHandler::getInstance()
	{
		return *instance;
	}
}
