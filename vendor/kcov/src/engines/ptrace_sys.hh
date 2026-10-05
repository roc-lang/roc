namespace ptrace_sys
{

int attachAll (pid_t pid);
int cont(pid_t pid, int signal);
void detach(pid_t pid);
bool disable_aslr(void);
// Does this event relate to creating new processes or threads?
bool eventIsForky(int signal, int event);
// Does this event come from a newly created, traced, child?
bool eventIsNewChild(int signal, int event);
int follow_child(pid_t pid);
int follow_fork(pid_t pid);
int get_current_cpu(void);
// Get the event that caused a trap.  The event is opaque to the caller.
int getEvent(pid_t pid, int status);
unsigned long getPc(int pid);
unsigned long peekWord(pid_t pid, unsigned long aligned_addr);
void pokeWord(pid_t pid, unsigned long aligned_addr, unsigned long value);
void singleStep(pid_t pid);
void skipInstruction(pid_t pid);
void tie_process_to_cpu(pid_t pid, int cpu);
int trace_me(void);
int wait_all(int *status);

}
