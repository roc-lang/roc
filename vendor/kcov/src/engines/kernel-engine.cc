#include <engine.hh>
#include <configuration.hh>
#include <output-handler.hh>
#include <utils.hh>
#include <zlib.h>

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>

#include <list>
#include <unordered_map>

using namespace kcov;

class KernelEngine : public IEngine
{
public:
	KernelEngine() :
			m_control(NULL), m_show(NULL), m_listener(NULL)
	{
	}

	~KernelEngine()
	{
		kill(0);
	}

	// From IEngine
	int registerBreakpoint(unsigned long addr)
	{
		// Don't register the same BP twice
		if (m_addresses.find(addr) != m_addresses.end())
			return 0;

		m_addresses[addr] = true;

		std::string s = fmt("%s0x%llx\n", m_module.c_str(), (unsigned long long) addr);

		kcov_debug(ENGINE_MSG, "KNRL set BP at 0x%llx\n", (unsigned long long) addr);
		panic_if(!m_control, "Control file not open???");

		fprintf(m_control, "%s", s.c_str());
		fflush(m_control);

		return 0;
	}

	void setupAllBreakpoints()
	{
	}

	bool clearBreakpoint(const Event &ev)
	{
		return true;
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		std::string path = IConfiguration::getInstance().keyAsString("kernel-coverage-path");
		std::string control = path + "/control";
		std::string show = path + "/show";

		m_listener = &listener;

		// Open kprobe-coverage files
		m_control = fopen(control.c_str(), "w");
		m_show = fopen(show.c_str(), "r");

		if (!m_control || !m_show)
		{
			error("Can't open kprobe-coverage files. Is the kprobe-coverage module loaded?");

			kill(0);
			return false;
		}

		return true;
	}

	bool continueExecution()
	{
		char buf[256]; // More than enough for one line
		char *p;

		p = fgets(buf, sizeof(buf) - 1, m_show);
		if (!p)
			return false;

		buf[sizeof(buf) - 1] = '\0';
		buf[strlen(buf) - 1] = '\0'; // Remove the \n

		parseOneLine(buf);

		return true;
	}

	void kill(int sig)
	{
		if (m_control)
		{
			fprintf(m_control, "clear\n");
			fclose(m_control);
		}
		if (m_show)
			fclose(m_show);

		m_control = NULL;
		m_show = NULL;
	}

private:
	void parseOneLine(const std::string &line)
	{
		std::string moduleName;
		std::string addr;

		size_t pos = line.find(":");

		if (pos != std::string::npos)
		{
			moduleName = line.substr(0, pos);
			addr = line.substr(pos + 1);
		}
		else
		{
			addr = line;
		}

		// Invalid?
		if (!string_is_integer(addr, 16))
			return;

		uint64_t value = string_to_integer(addr, 16);

		kcov_debug(ENGINE_MSG, "KNRL BP at 0x%llx\n", (unsigned long long) value);

		m_listener->onEvent(Event(ev_breakpoint, 0, value));
	}

	FILE *m_control;
	FILE *m_show;
	IEventListener *m_listener;

	std::unordered_map<unsigned long, bool> m_addresses;

	std::string m_module;
};

class KernelEngineCreator : public IEngineFactory::IEngineCreator
{
public:
	virtual ~KernelEngineCreator()
	{
	}

	virtual IEngine *create()
	{
		return new KernelEngine();
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		if (filename.find("vmlinux") != std::string::npos)
			return match_perfect;

		if (filename.find(".ko") == std::string::npos)
			return match_none;

		// Fix this somehow
#if 0
		m_module = filename;
		size_t slashPos = m_module.rfind("/");
		if (slashPos == std::string::npos)
		slashPos = 0;
		else
		slashPos++; // Skip the actual slash

		// Remove path before and .ko after the name
		m_module = m_module.substr(slashPos, m_module.size() - slashPos - 3) + ":";
#endif

		return match_perfect;
	}
};
static KernelEngineCreator g_kernelEngineCreator;
