#include <sys/types.h>

#include <engine.hh>
#include <utils.hh>
#include <configuration.hh>
#include <output-handler.hh>
#include <solib-handler.hh>
#include <file-parser.hh>
#include <libelf.h>
#include <phdr_data.h>
#include "ptrace_sys.hh"

#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>

#include <map>
#include <unordered_map>
#include <list>
#include <mutex>
#include <vector>

using namespace kcov;

#define str(s) #s
#define xstr(s) str(s)

static unsigned long getAligned(unsigned long addr)
{
	return (addr / sizeof(unsigned long)) * sizeof(unsigned long);
}

static unsigned long arch_setupBreakpoint(unsigned long addr, unsigned long old_data)
{
	unsigned long val;

#if defined(__i386__) || defined(__x86_64__)
	unsigned long aligned_addr = getAligned(addr);
	unsigned long offs = addr - aligned_addr;
	unsigned long shift = 8 * offs;

	val = (old_data & ~(0xffUL << shift)) | (0xccUL << shift);
#elif defined(__powerpc__)
	val = 0x7fe00008; /* tw */
#elif defined(__arm__)
	val = 0xfedeffe7; // Undefined insn
#elif defined(__aarch64__)
	unsigned long aligned_addr = getAligned(addr);
	unsigned long offs = addr - aligned_addr;
	unsigned long shift = 8 * offs;

	val = (old_data & ~(0xffffffffUL << shift)) | (0xd4200000UL << shift);
#elif defined(__riscv)
	val = 0x00100073; /* ebreak */ // No width problem, prefer ebreak than c.ebreak for ISA w/o C extension.
#elif defined(__loongarch__)
	unsigned long aligned_addr = getAligned(addr);
	unsigned long offs = addr - aligned_addr;
	unsigned long shift = 8 * offs;

	val = (old_data & ~(0xffffffffUL << shift)) | (0x002a0004UL << shift); /* break 0x4 */
#else
# error Unsupported architecture
#endif

	return val;
}

static unsigned long arch_clearBreakpoint(unsigned long addr, unsigned long old_data, unsigned long cur_data)
{
	unsigned long val;
#if defined(__i386__) || defined(__x86_64__)
	unsigned long aligned_addr = getAligned(addr);
	unsigned long offs = addr - aligned_addr;
	unsigned long shift = 8 * offs;
	unsigned long old_byte = (old_data >> shift) & 0xffUL;

	val = (cur_data & ~(0xffUL << shift)) | (old_byte << shift);
#else
	val = old_data;
#endif

	return val;
}

class Ptrace : public IEngine
{
public:
	Ptrace() : m_firstBreakpoint(true), m_activeChild(0), m_child(0), m_firstChild(0),
		m_parentCpu(0), m_listener(NULL), m_signal(0)
	{
	}

	~Ptrace()
	{
		kill(SIGTERM);
		ptrace_sys::detach(m_activeChild);
	}

	bool start(IEventListener &listener, const std::string &executable)
	{
		m_listener = &listener;

		m_parentCpu = ptrace_sys::get_current_cpu();
		ptrace_sys::tie_process_to_cpu(getpid(), m_parentCpu);

		m_instructionMap.clear();

		/* Basic check first */
		if (access(executable.c_str(), X_OK) != 0)
			return false;

		unsigned int pid = IConfiguration::getInstance().keyAsInt("attach-pid");
		bool res = false;

		if (pid != 0)
			res = attachPid(pid);
		else
			res = forkChild(executable.c_str());

		return res;
	}

	int registerBreakpoint(unsigned long addr)
	{
		if (addr == 0)
			return -1;

		unsigned long data;

		// There already?
		if (m_instructionMap.find(addr) != m_instructionMap.end())
			return 0;

		data = ptrace_sys::peekWord(m_activeChild, getAligned(addr));

		m_instructionMap[addr] = data;
		m_pendingBreakpoints.push_back(addr);

		kcov_debug(BP_MSG, "BP registered at 0x%lx\n", addr);

		return 0;
	}

	bool clearBreakpoint(unsigned long addr)
	{
		if (m_instructionMap.find(addr) == m_instructionMap.end())
		{
			kcov_debug(BP_MSG, "Can't find breakpoint at 0x%lx\n", addr);

			// Stupid workaround for avoiding the solib thread race
			msleep(1);

			return false;
		}

		// Clear the actual breakpoint instruction
		unsigned long val = m_instructionMap[addr];
		val = arch_clearBreakpoint(addr, val, ptrace_sys::peekWord(m_activeChild, getAligned(addr)));

		ptrace_sys::pokeWord(m_activeChild, getAligned(addr), val);

		return true;
	}

	const Event waitEvent()
	{
		static uint64_t lastSignalAddress;
		Event out;
		int status;
		pid_t who;

		// Assume error
		out.type = ev_error;
		out.data = -1;

		who = ptrace_sys::wait_all(&status);

		if (who == -1)
		{
			kcov_debug(ENGINE_MSG, "Returning error\n");
			return out;
		}

		m_children[who] = 1;

		m_activeChild = who;
		out.addr = ptrace_sys::getPc(m_activeChild);

		kcov_debug(ENGINE_MSG, "PT stopped PID %d 0x%08x\n", m_activeChild, status);

		// A signal?
		if (WIFSTOPPED(status))
		{
			int sig = WSTOPSIG(status);
			int sigill = SIGSYS;

			// Arm is using an undefined instruction, so we'll get a SIGILL here
#if defined(__arm__) || defined(__aarch64__)
			sigill = SIGILL;
#endif

			out.type = ev_signal;
			out.data = sig;
			int event = ptrace_sys::getEvent(who, status);
			if (ptrace_sys::eventIsNewChild(sig, event))
			{
				// Activate tracing on a new child
				kcov_debug(ENGINE_MSG, "PT new child %d\n", m_activeChild);
				ptrace_sys::follow_child(m_activeChild);
				out.data = 0;
			}
			else if (ptrace_sys::eventIsForky(sig, event))
			{
				kcov_debug(ENGINE_MSG, "PT fork/clone at 0x%llx for %d\n", (unsigned long long) out.addr, m_activeChild);
				out.data = 0;
			}
			else if (sig == SIGTRAP || sig == SIGSTOP || sig == sigill)
			{
				// A trap?
				out.type = ev_breakpoint;
				out.data = -1;

				kcov_debug(ENGINE_MSG, "PT BP at 0x%llx:%d for %d\n", (unsigned long long) out.addr, out.data,
						m_activeChild);

				bool insnFound = m_instructionMap.find(out.addr) != m_instructionMap.end();

				// Single-step if we have this BP
				if (insnFound)
					ptrace_sys::singleStep(m_activeChild);
				else if (sig != SIGSTOP)
					ptrace_sys::skipInstruction(m_activeChild);

				// Wait for solib data if this is the first time
				if (m_firstBreakpoint && insnFound)
				{
					blockUntilSolibDataRead();
					m_firstBreakpoint = false;
				}

				return out;
			}

			kcov_debug(ENGINE_MSG, "PT signal %d at 0x%llx for %d\n", WSTOPSIG(status), (unsigned long long) out.addr,
					m_activeChild);
			lastSignalAddress = out.addr;
		}
		else if (WIFSIGNALED(status))
		{
			// Crashed/killed
			int sig = WTERMSIG(status);

			out.type = ev_signal;
			out.data = sig;
			out.addr = lastSignalAddress;

			kcov_debug(ENGINE_MSG, "PT terminating signal %d at 0x%llx for %d\n", sig, (unsigned long long) out.addr,
					m_activeChild);
			m_children.erase(who);

			if (!childrenLeft())
				out.type = ev_signal_exit;

		}
		else if (WIFEXITED(status))
		{
			int exitStatus = WEXITSTATUS(status);

			kcov_debug(ENGINE_MSG, "PT exit %d at 0x%llx for %d%s\n", exitStatus, (unsigned long long) out.addr,
					m_activeChild, m_activeChild == m_firstChild ? " (first child)" : "");

			m_children.erase(who);

			if (who == m_firstChild)
				out.type = ev_exit_first_process;
			else
				out.type = ev_exit;

			out.data = exitStatus;
		}

		return out;
	}

	bool childrenLeft()
	{
		return m_children.size() > 0 && m_children.find(m_firstChild) != m_children.end();
	}

	/**
	 * Continue execution with an event
	 */
	bool continueExecution()
	{
		int res;

		setupAllBreakpoints();

		kcov_debug(ENGINE_MSG, "PT continuing %d with signal %d\n", m_activeChild, m_signal);
		res = ptrace_sys::cont(m_activeChild, m_signal);
		if (res < 0)
		{
			kcov_debug(ENGINE_MSG, "PT error for %d: %d\n", m_activeChild, res);
			m_children.erase(m_activeChild);
		}

		Event ev = waitEvent();
		m_signal = ev.type == ev_signal ? ev.data : 0;

		if (m_listener)
			m_listener->onEvent(ev);

		if (ev.type == ev_breakpoint)
			clearBreakpoint(ev.addr);

		if (ev.type == ev_error)
			return false;

		return true;
	}

	void kill(int signal)
	{
		// Don't kill kcov itself (PID 0)
		if (m_activeChild != 0)
			::kill(m_activeChild, signal);
	}

private:

	void setupAllBreakpoints()
	{
		for (PendingBreakpointList_t::const_iterator addrIt = m_pendingBreakpoints.begin();
				addrIt != m_pendingBreakpoints.end(); ++addrIt)
		{
			unsigned long addr = *addrIt;
			unsigned long cur_data = ptrace_sys::peekWord(m_activeChild, getAligned(addr));

			// Set the breakpoint
			ptrace_sys::pokeWord(m_activeChild, getAligned(addr), arch_setupBreakpoint(addr, cur_data));
		}

		m_pendingBreakpoints.clear();
	}

	bool forkChild(const char *executable)
	{
		char * const *argv = (char * const *) IConfiguration::getInstance().getArgv();
		pid_t child, who;
		int status;

		/* Executable exists, try to launch it */
		if ((child = fork()) == 0)
		{
			int res;

			/* Avoid address randomization */
			if (!ptrace_sys::disable_aslr())
				return false;

			/* And launch the process */
			res = ptrace_sys::trace_me();
			if (res < 0)
			{
				perror("Can't set me as ptraced");
				return false;
			}
			ptrace_sys::tie_process_to_cpu(getpid(), m_parentCpu);
			execv(executable, argv);

			/* Exec failed */
			return false;
		}

		/* Fork error? */
		if (child < 0)
		{
			perror("fork");
			return false;
		}
		m_child = m_activeChild = m_firstChild = child;
		// Might not be completely necessary (the child should inherit this
		// from the parent), but better safe than sorry
		ptrace_sys::tie_process_to_cpu(m_child, m_parentCpu);

		kcov_debug(ENGINE_MSG, "PT forked %d\n", child);

		/* Wait for the initial stop */
		who = waitpid(child, &status, 0);
		if (who < 0)
		{
			perror("waitpid");
			return false;
		}
		if (!WIFSTOPPED(status))
		{
			fprintf(stderr, "Child hasn't stopped: %x\n", status);
			return false;
		}

		ptrace_sys::follow_fork(m_activeChild);

		return true;
	}

	bool attachPid(pid_t pid)
	{
		int rv;

		m_child = m_activeChild = m_firstChild = pid;

		errno = 0;
		rv = ptrace_sys::attachAll(m_activeChild);
		//rv = ptrace(PTRACE_ATTACH, m_activeChild, 0, 0);
		if (rv < 0)
		{
			const char *err = strerror(errno);

			fprintf(stderr, "Can't attach to %d. Error %s\n", pid, err);
			return false;
		}

		/* Wait for the initial stop */
		int status;
		int who = waitpid(m_activeChild, &status, 0);
		if (who < 0)
		{
			perror("waitpid");
			return false;
		}
		if (!WIFSTOPPED(status))
		{
			fprintf(stderr, "Child hasn't stopped: %x\n", status);
			return false;
		}
		ptrace_sys::tie_process_to_cpu(m_activeChild, m_parentCpu);

		return true;
	}

	typedef std::unordered_map<unsigned long, unsigned long> instructionMap_t;
	typedef std::vector<unsigned long> PendingBreakpointList_t;
	typedef std::unordered_map<pid_t, int> ChildMap_t;

	instructionMap_t m_instructionMap;
	PendingBreakpointList_t m_pendingBreakpoints;
	bool m_firstBreakpoint;

	pid_t m_activeChild;
	pid_t m_child;
	pid_t m_firstChild;
	ChildMap_t m_children;

	int m_parentCpu;

	IEventListener *m_listener;
	int m_signal;
};

class PtraceEngineCreator : public IEngineFactory::IEngineCreator
{
public:
	virtual ~PtraceEngineCreator()
	{
	}

	virtual IEngine *create()
	{
		return new Ptrace();
	}

	unsigned int matchFile(const std::string &filename, uint8_t *data, size_t dataSize)
	{
		Elf32_Ehdr *hdr = (Elf32_Ehdr *) data;

		if (memcmp(hdr->e_ident, ELFMAG, strlen(ELFMAG)) == 0)
		{
			return match_perfect;
		}

		// Unless #!/bin/sh etc, this should win
		return 1;
	}
};

static PtraceEngineCreator g_ptraceEngineCreator;
