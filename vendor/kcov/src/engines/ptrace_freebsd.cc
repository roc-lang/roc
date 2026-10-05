#include <sys/types.h>
#include <sys/ptrace.h>
#include <sys/wait.h>

#include <errno.h>

#include "ptrace_sys.hh"

static void arch_adjustPcAfterBreakpoint(struct reg *regs);
unsigned long arch_getPcFromRegs(struct reg *regs);
static unsigned long getPcFromRegs(struct reg *regs);
static long getRegs(pid_t pid, void *addr, struct reg *regs);
static long setRegs(pid_t pid, void *addr, struct reg *regs);


static void
arch_adjustPcAfterBreakpoint(struct reg *regs)
{
#if defined(__x86_64__)
	regs->r_rip--;
#else
# error Unsupported architecture
#endif
}

unsigned long arch_getPcFromRegs(struct reg *regs)
{
#if defined(__x86_64__)
	return(regs->r_rip) - 1;
#else
# error Unsupported architecture
#endif
}

int
ptrace_sys::attachAll (pid_t pid)
{
	return (ptrace(PT_ATTACH, pid, 0, 0) < 0 ? errno: 0);
}

int
ptrace_sys::cont(pid_t pid, int signal)
{
	return ptrace(PT_CONTINUE, pid, (caddr_t)1, signal);
}

void
ptrace_sys::detach(pid_t pid)
{
	ptrace(PT_DETACH, pid, 0, 0);
}

bool
ptrace_sys::disable_aslr(void)
{
	return true;
}

bool
ptrace_sys::eventIsForky(int signal, int event)
{
	return (signal == SIGTRAP && (event & PL_FLAG_FORKED));
}

bool
ptrace_sys::eventIsNewChild(int signal, int event)
{
	return (signal == SIGSTOP && (event & PL_FLAG_CHILD));
}

int
ptrace_sys::follow_child(pid_t pid)
{
	return ptrace_sys::follow_fork(pid);
}

int
ptrace_sys::follow_fork(pid_t pid)
{
	return ptrace(PT_FOLLOW_FORK, pid, NULL, 1);
}

int
ptrace_sys::getEvent(pid_t pid, int status)
{
	struct ptrace_lwpinfo lwpinfo;
	ptrace(PT_LWPINFO, pid, (caddr_t)&lwpinfo, sizeof(lwpinfo));
	return (lwpinfo.pl_flags);
}

unsigned long
ptrace_sys::getPc(int pid)
{
	struct reg regs;
	getRegs(pid, NULL, &regs);
	return getPcFromRegs(&regs);
}

int
ptrace_sys::get_current_cpu(void)
{
	return 0;
}

static unsigned long
getPcFromRegs(struct reg *regs)
{
	return arch_getPcFromRegs(regs);
}

static long
getRegs(pid_t pid, void *addr, struct reg *regs)
{
	return ptrace(PT_GETREGS, pid, (caddr_t)regs, 0);
}

unsigned long
ptrace_sys::peekWord(pid_t pid, unsigned long aligned_addr)
{
	unsigned long val;

	struct ptrace_io_desc ptiod =
	{
		.piod_op = PIOD_READ_I,
		.piod_offs = (void*)aligned_addr,
		.piod_addr = &val,
		.piod_len = sizeof(val)
	};
	ptrace(PT_IO, pid, (caddr_t)&ptiod, 0);
	return val;
}

void
ptrace_sys::pokeWord(pid_t pid, unsigned long aligned_addr, unsigned long value)
{
	struct ptrace_io_desc ptiod =
	{
		.piod_op = PIOD_WRITE_I,
		.piod_offs = (void*)aligned_addr,
		.piod_addr = &value,
		.piod_len = sizeof(value)
	};
	ptrace(PT_IO, pid, (caddr_t)&ptiod, 0);
}

static long
setRegs(pid_t pid, void *addr, struct reg *regs)
{
	return ptrace(PT_SETREGS, pid, (caddr_t)regs, 0);
}

void
ptrace_sys::singleStep(pid_t pid)
{
	// Step back one instruction
	struct reg regs;
	getRegs(pid, NULL, &regs);
	arch_adjustPcAfterBreakpoint(&regs);
	setRegs(pid, NULL, &regs);
}

// Skip over this instruction
void
ptrace_sys::skipInstruction(pid_t pid)
{
	// Nop on x86 and x86_64
}

void
ptrace_sys::tie_process_to_cpu(pid_t pid, int cpu)
{
}

int
ptrace_sys::trace_me(void)
{
	return ptrace(PT_TRACE_ME, 0, 0, 0);
}

pid_t
ptrace_sys::wait_all(int *status)
{
	return waitpid(-1, status, WSTOPPED);
}
