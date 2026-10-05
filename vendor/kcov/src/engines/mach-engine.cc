/**
 * @file
 * @brief An engine for Mach (OSX)
 *
 * Based on
 *
 * - https://github.com/danylokos/vm-demo (writing memory for setting breakpoints)
 * - https://www.spaceflint.com/?p=150 (ptrace, mach exceptions)
 * - http://uninformed.org/index.cgi?v=4&a=3&p=14 (reading registers)
 * - https://github.com/PsychoBird/Machium (reading registers, for breakpoints)
 */
#include "osx/mach_exc.h"

#include <configuration.hh>
#include <cstddef>
#include <engine.hh>
#include <mach-o/loader.h>
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_init.h>
#include <mach/mach_types.h>
#include <mach/port.h>
#include <mach/task.h>
#include <mach/thread_status.h>
#include <mach/vm_map.h>
#include <mach/vm_page_size.h>
#include <set>
#include <signal.h>
#include <spawn.h>
#include <sys/errno.h>
// clang-format off
// sys/ptrace.h needs sys/types.h, so make sure clang-format doesn't change the order
#include <sys/types.h>
#include <sys/ptrace.h>
// clang-format on
#include <cassert>
#include <iostream>
#include <unistd.h>
#include <unordered_map>
#include <utils.hh>

using namespace kcov;
extern char** environ;

extern "C" boolean_t mach_exc_server(mach_msg_header_t* InHeadP, mach_msg_header_t* OutHeadP);

namespace
{

// See x86_thread_state64_t and arm_thread_state64_t
#if defined(__x86_64__)
constexpr auto x86_rip_offset = 16;

static_assert(offsetof(x86_thread_state64_t, __rip) == x86_rip_offset * sizeof(uint64_t));

constexpr auto pc_offset = x86_rip_offset;
constexpr auto regs_flavor = x86_THREAD_STATE64;

#elif defined(__arm64__)
constexpr auto arm64_pc_offset = 32;

static_assert(offsetof(arm_thread_state64_t, __pc) == arm64_pc_offset * sizeof(uint64_t));

constexpr auto pc_offset = arm64_pc_offset;
constexpr auto regs_flavor = ARM_THREAD_STATE64;

#else
#error unknown architecture
#endif


constexpr uint32_t
getAligned(uint32_t addr)
{
    return (addr / sizeof(uint32_t)) * sizeof(uint32_t);
}

unsigned long
arch_setupBreakpoint(unsigned long addr, unsigned long old_data)
{
    unsigned long val;

#if defined(__i386__) || defined(__x86_64__)
    unsigned long aligned_addr = getAligned(addr);
    unsigned long offs = addr - aligned_addr;
    unsigned long shift = 8 * offs;

    val = (old_data & ~(0xffUL << shift)) | (0xccUL << shift);
#elif defined(__arm64__)
    unsigned long aligned_addr = getAligned(addr);
    unsigned long offs = addr - aligned_addr;
    unsigned long shift = 8 * offs;

    val = (old_data & ~(0xffffffffUL << shift)) | (0xd4200000UL << shift);
#else
#error Unsupported architecture
#endif

    return val;
}

unsigned long
arch_clearBreakpoint(unsigned long addr, unsigned long old_data, unsigned long cur_data)
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


bool
arch_adjustPcAfterBreakpoint(uint64_t* regs)
{
#if defined(__x86_64__)
    regs[pc_offset]--;

    return true;
#endif

    return false;
}


} // namespace


static void onSigchld(int sig);

class MachEngine : public IEngine
{
public:
    MachEngine()
    {
    }


    void childExit()
    {
        // Deallocate the mach port to let the syscall in continueExecution loose
        mach_port_mod_refs(mach_task_self(), target_exception_port, MACH_PORT_RIGHT_RECEIVE, -1);
        mach_port_deallocate(mach_task_self(), target_exception_port);
    }

    // Callback from the mig stuff (from https://www.spaceflint.com/?p=150)
    kern_return_t CatchMachExceptionRaise(mach_port_t exception_port,
                                          mach_port_t thread_port,
                                          mach_port_t task_port,
                                          exception_type_t exception_type,
                                          mach_exception_data_t codes,
                                          mach_msg_type_number_t num_codes)
    {
        if (exception_type == EXC_SOFTWARE && codes[0] == EXC_SOFT_SIGNAL)
        {
            // Forward signals
            ptrace(PT_THUPDATE, m_pid, (caddr_t)(uintptr_t)thread_port, codes[1]);
        }
        else if (exception_type == EXC_BREAKPOINT)
        {
            uint64_t regs[128];

            mach_msg_type_number_t state_count = THREAD_MACHINE_STATE_MAX;
            auto kret =
                thread_get_state(thread_port, regs_flavor, (thread_state_t)regs, &state_count);
            if (kret != KERN_SUCCESS)
            {
                error("thread_get_state with error: %s\n", mach_error_string(kret));
                return KERN_SUCCESS;
            }

            auto dirty = arch_adjustPcAfterBreakpoint(regs);
            auto pc = regs[pc_offset];

            m_listener->onEvent(IEngine::Event(ev_breakpoint, -1, pc));

            if (m_instructionMap.find(pc) != m_instructionMap.end())
            {
                clearBreakpoint(pc);
            }

            if (dirty)
            {
                kret =
                    thread_set_state(thread_port, regs_flavor, (thread_state_t)regs, state_count);
                if (kret != KERN_SUCCESS)
                {
                    error("thread_set_state with error: %s\n", mach_error_string(kret));
                    return KERN_SUCCESS;
                }
            }
        }

        return KERN_SUCCESS;
    }

private:
    // From IEngine
    virtual int registerBreakpoint(unsigned long addr) override
    {
        uint32_t data = 0;

        // There already?
        if (m_instructionMap.find(addr) != m_instructionMap.end())
            return 0;

        if (IConfiguration::getInstance().keyAsInt("running-mode") !=
            IConfiguration::MODE_REPORT_ONLY)
        {
            data = peekWord(getAligned(addr));
        }
        m_instructionMap[addr] = data;
        m_pendingBreakpoints.insert(addr);

        kcov_debug(BP_MSG, "BP registered at 0x%lx\n", addr);

        return m_instructionMap.size();
    }


    bool start(IEventListener& listener, const std::string& executable) final
    {
        m_listener = &listener;

        if (IConfiguration::getInstance().keyAsInt("attach-pid") != 0)
        {
            error("the mach-engine does not support --pid (on OSX)");
            exit(1);
        }


        posix_spawnattr_t attr;

        auto rv = posix_spawnattr_init(&attr);
        if (rv != 0)
        {
            error("posix_spawnattr_init");
            return false;
        }

        // Start suspended without address space randomization
        rv = posix_spawnattr_setflags(&attr, POSIX_SPAWN_START_SUSPENDED | 0x100);
        if (rv != 0)
        {
            error("posix_spawnattr_setflags");
            return false;
        }

        // Fork the process, in suspended mode
        auto& conf = IConfiguration::getInstance();
        auto argv = conf.getArgv();
        rv = posix_spawn(&m_pid, argv[0], nullptr, &attr, (char* const*)argv, environ);
        if (rv != 0)
        {
            error("posix_spawn");
            return false;
        }

        posix_spawnattr_destroy(&attr);

        auto kret = task_for_pid(mach_task_self(), m_pid, &m_task);
        if (kret != KERN_SUCCESS)
        {
            auto kcov_path = get_real_path(conf.keyAsString("kcov-binary-path"));

            error("task_for_pid failed with %d\n"
                  "\n"
                  "This usually means that the kcov binary needs to be signed with codesign, when\n"
                  "not running as root. It should be enough to run\n"
                  "\n"
                  "  codesign -s - --entitlements <kcov-source>/osx-entitlements.xml -f %s\n"
                  "\n"
                  "See "
                  "https://github.com/SimonKagstrom/kcov/blob/master/INSTALL.md for more\n"
                  "instructions.\n",
                  kret,
                  kcov_path.c_str());

            exit(1);
            return false;
        }

        if (!conf.keyAsInt("is-go-binary"))
        {
            m_imageBase = findImageAddress();
        }

        rv = task_get_exception_ports(m_task,
                                      EXC_MASK_ALL,
                                      saved_masks,
                                      &saved_exception_types_count,
                                      saved_ports,
                                      saved_behaviors,
                                      saved_flavors);
        if (rv != 0)
        {
            error("task_get_exception_ports: %d\n", rv);
            return false;
        }

        // allocate and authorize a new port
        rv = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &target_exception_port);
        if (rv != 0)
        {
            error("mach_port_allocate: %d\n", rv);
            return false;
        }

        rv = mach_port_insert_right(mach_task_self(),
                                    target_exception_port,
                                    /* and again */ target_exception_port,
                                    MACH_MSG_TYPE_MAKE_SEND);
        if (rv != 0)
        {
            error("mach_port_insert_right: %d\n", rv);
        }

        // Receive exceptions (signals + breakpoints) from the debugged process
        rv = task_set_exception_ports(m_task,
                                      EXC_MASK_ALL & ~EXC_MASK_RESOURCE,
                                      target_exception_port,
                                      EXCEPTION_DEFAULT | MACH_EXCEPTION_CODES,
                                      THREAD_STATE_NONE);
        if (rv != 0)
        {
            error("task_set_exception_ports: %d\n", rv);
            return false;
        }

        // Handle when the child exits (yes, ugly...)
        signal(SIGCHLD, onSigchld);

        rv = ptrace(PT_ATTACHEXC, m_pid, 0, 0);
        if (rv != 0)
        {
            error("ptrace error %d, errno %d\n", rv, errno);
            return false;
        }


        return true;
    }

    bool continueExecution() final
    {
        setupAllBreakpoints();

        char req[128], rpl[128]; /* request and reply buffers */

        // wait indefinitely to receive an exception message
        auto krt = mach_msg((mach_msg_header_t*)req, /* receive buffer */
                            MACH_RCV_MSG,            /* receive message */
                            0,                       /* size of send buffer */
                            sizeof(req),             /* size of receive buffer */
                            target_exception_port,   /* port to receive on */
                            MACH_MSG_TIMEOUT_NONE,   /* wait indefinitely */
                            MACH_PORT_NULL);         /* notify port, unused */

        if (krt == KERN_SUCCESS)
        {
            task_suspend(m_task);

            /* mach_exc_server calls catch_mach_exception_raise */

            boolean_t message_parsed_correctly =

                mach_exc_server((mach_msg_header_t*)req, (mach_msg_header_t*)rpl);

            if (!message_parsed_correctly)
            {
                error("mach_ex_server parse error\n");
                //kret_from_catch_mach_exception_raise = ((mig_reply_error_t*)rpl)->RetCode;
            }
        }
        else
        {
            m_listener->onEvent(IEngine::Event(ev_exit, 0));
            return false;
        }

        /* resume all threads in the process before replying to the exception */
        task_resume(m_task);
        /* reply to the exception */

        mach_msg_size_t send_sz = ((mach_msg_header_t*)rpl)->msgh_size;

        mach_msg((mach_msg_header_t*)rpl, /* send buffer */
                 MACH_SEND_MSG,           /* send message */
                 send_sz,                 /* size of send buffer */
                 0,                       /* size of receive buffer */
                 MACH_PORT_NULL,          /* port to receive on */
                 MACH_MSG_TIMEOUT_NONE,   /* wait indefinitely */
                 MACH_PORT_NULL);         /* notify port, unused */

        return true;
    }

    void kill(int signal) final
    {
        ::kill(m_pid, signal);
    }

    // From vm-demo.git
    vm_address_t findImageAddress()
    {
        kern_return_t kr;
        vm_address_t image_addr = 0;
        int headers_found = 0;
        vm_address_t addr = 0;
        vm_size_t size;
        vm_region_submap_info_data_64_t info;
        mach_msg_type_number_t info_count = VM_REGION_SUBMAP_INFO_COUNT_64;
        unsigned int depth = 0;

        while (true)
        {
            // get next memory region
            kr = vm_region_recurse_64(
                m_task, &addr, &size, &depth, (vm_region_info_t)&info, &info_count);
            if (kr != KERN_SUCCESS)
                break;
            unsigned int header;
            vm_size_t bytes_read;

            // As in vm-demo.git, the error handling here should be "improved"
            kr = vm_read_overwrite(m_task, addr, 4, (vm_address_t)&header, &bytes_read);
            if (kr != KERN_SUCCESS)
            {
                error("vm_read_overwrite failed\n");
                exit(-1);
            }
            if (bytes_read != 4)
            {
                exit(-1);
            }
            if (header == MH_MAGIC_64)
            {
                headers_found++;
            }
            if (headers_found == 1)
            {
                image_addr = addr;
                break;
            }
            addr += size;
        }
        if (image_addr == 0)
        {
            exit(-1);
        }

        return image_addr;
    }

    void setupAllBreakpoints()
    {
        for (auto addr : m_pendingBreakpoints)
        {
            unsigned long cur_data = peekWord(getAligned(addr));

            // Set the breakpoint
            pokeWord(getAligned(addr), arch_setupBreakpoint(addr, cur_data));
        }

        m_pendingBreakpoints.clear();
    }


    bool clearBreakpoint(uint64_t addr)
    {
        if (m_instructionMap.find(addr) == m_instructionMap.end())
        {
            kcov_debug(BP_MSG, "Can't find breakpoint at 0x%llx\n", addr);

            return false;
        }

        // Clear the actual breakpoint instruction
        auto val = m_instructionMap[addr];
        val = arch_clearBreakpoint(addr, val, peekWord(getAligned(addr)));

        pokeWord(getAligned(addr), val);

        return true;
    }

    uint32_t peekWord(uint64_t aligned_addr)
    {
        assert((aligned_addr & 3) == 0 && "The peeked address must be aligned");
        uint32_t val = 0;
        size_t size = 0;

        // FIXME! Is this really true?
        auto patch_addr = m_imageBase + (aligned_addr & 0xffffffff);

        auto kr = vm_read_overwrite(m_task, patch_addr, sizeof(val), (vm_offset_t)&val, &size);
        if (kr != KERN_SUCCESS)
        {
            panic("vm_read_overwrite failed for peekWord for addr 0x%llx, msg: %s",
                  patch_addr,
                  mach_error_string(kr));
            return 0;
        }

        return val;
    }

    void pokeWord(uint64_t aligned_addr, uint32_t value)
    {
        assert((aligned_addr & 3) == 0 && "The poked address must be aligned");

        // FIXME! Is this really true?
        auto patch_addr = m_imageBase + (aligned_addr & 0xffffffff);

        // VM_PROT_COPY forces COW, probably, see vm_map_protect in vm_map.c
        kern_return_t kr;
        kr = vm_protect(m_task,
                        trunc_page(patch_addr),
                        vm_page_size,
                        false,
                        VM_PROT_READ | VM_PROT_WRITE | VM_PROT_COPY);
        if (kr != KERN_SUCCESS)
        {
            printf("%s ", mach_error_string(kr));
            panic("vm_protect failed\n");
        }

        kr = vm_write(m_task, patch_addr, (vm_offset_t)&value, sizeof(value));
        if (kr != KERN_SUCCESS)
        {
            error("vm_write failed\n");
            return;
        }
        kr = vm_protect(
            m_task, trunc_page(patch_addr), vm_page_size, false, VM_PROT_READ | VM_PROT_EXECUTE);
        if (kr != KERN_SUCCESS)
        {
            printf("%s ", mach_error_string(kr));
            panic("vm_protect failed\n");
        }
    }

private:
    typedef std::unordered_map<unsigned long, unsigned long> InstructionMap_t;

    std::unordered_map<uint64_t, uint32_t> m_instructionMap;
    std::set<uint64_t> m_pendingBreakpoints;
    IEventListener* m_listener {nullptr};
    pid_t m_pid {0};
    task_t m_task {MACH_PORT_NULL};
    uint64_t m_imageBase {0};

    exception_mask_t saved_masks[EXC_TYPES_COUNT];
    mach_port_t saved_ports[EXC_TYPES_COUNT];
    exception_behavior_t saved_behaviors[EXC_TYPES_COUNT];
    thread_state_flavor_t saved_flavors[EXC_TYPES_COUNT];
    mach_msg_type_number_t saved_exception_types_count;
    mach_port_name_t target_exception_port;
};


static MachEngine* g_machEngine;

extern "C" kern_return_t catch_mach_exception_raise(mach_port_t exception_port,
                                                    mach_port_t thread_port,
                                                    mach_port_t task_port,
                                                    exception_type_t exception_type,
                                                    mach_exception_data_t codes,
                                                    mach_msg_type_number_t num_codes);

extern "C" kern_return_t catch_mach_exception_raise_state(mach_port_t exception_port,
                                                          exception_type_t exception,
                                                          const mach_exception_data_t code,
                                                          mach_msg_type_number_t codeCnt,
                                                          int* flavor,
                                                          const thread_state_t old_state,
                                                          mach_msg_type_number_t old_stateCnt,
                                                          thread_state_t new_state,
                                                          mach_msg_type_number_t* new_stateCnt);

extern "C" kern_return_t
catch_mach_exception_raise_state_identity(mach_port_t exception_port,
                                          mach_port_t thread,
                                          mach_port_t task,
                                          exception_type_t exception,
                                          mach_exception_data_t code,
                                          mach_msg_type_number_t codeCnt,
                                          int* flavor,
                                          thread_state_t old_state,
                                          mach_msg_type_number_t old_stateCnt,
                                          thread_state_t new_state,
                                          mach_msg_type_number_t* new_stateCnt);


kern_return_t
catch_mach_exception_raise(mach_port_t exception_port,
                           mach_port_t thread_port,
                           mach_port_t task_port,
                           exception_type_t exception_type,
                           mach_exception_data_t codes,
                           mach_msg_type_number_t num_codes)
{
    return g_machEngine->CatchMachExceptionRaise(
        exception_port, thread_port, task_port, exception_type, codes, num_codes);
}

/* catch_mach_exception_raise_state */

kern_return_t
catch_mach_exception_raise_state(mach_port_t exception_port,
                                 exception_type_t exception,
                                 const mach_exception_data_t code,
                                 mach_msg_type_number_t codeCnt,
                                 int* flavor,
                                 const thread_state_t old_state,
                                 mach_msg_type_number_t old_stateCnt,
                                 thread_state_t new_state,
                                 mach_msg_type_number_t* new_stateCnt)
{
    /* not used because EXCEPTION_STATE is not specified in the call */
    /* to task_set_exception_ports, but referenced by mach_exc_server */

    return MACH_RCV_INVALID_TYPE;
}

/* catch_mach_exception_raise_state_identity */

kern_return_t
catch_mach_exception_raise_state_identity(mach_port_t exception_port,
                                          mach_port_t thread,
                                          mach_port_t task,
                                          exception_type_t exception,
                                          mach_exception_data_t code,
                                          mach_msg_type_number_t codeCnt,
                                          int* flavor,
                                          thread_state_t old_state,
                                          mach_msg_type_number_t old_stateCnt,
                                          thread_state_t new_state,
                                          mach_msg_type_number_t* new_stateCnt)
{
    /* not used because EXCEPTION_STATE_IDENTITY is not specified in the   */
    /* call to task_set_exception_ports, but referenced by mach_exc_server */

    return MACH_RCV_INVALID_TYPE;
}

void
onSigchld(int sig)
{
    g_machEngine->childExit();
}

class MachEngineCreator : public IEngineFactory::IEngineCreator
{
public:
    virtual ~MachEngineCreator()
    {
    }

    virtual IEngine* create()
    {
        g_machEngine = new MachEngine();
        return g_machEngine;
    }

    unsigned int matchFile(const std::string& filename, uint8_t* data, size_t dataSize)
    {
        // Unless #!/bin/sh etc, this should win
        return 2;
    }
};

static MachEngineCreator g_machEngineCreator;
