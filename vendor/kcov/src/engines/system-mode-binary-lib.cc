#include <sys/types.h>
#include <time.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <utils.hh>

#include <system-mode/registration.hh>

void connectToDaemon(void) __attribute__((constructor));

void connectToDaemon(void)
{
	std::string destinationPipe = "/tmp/kcov-system.pipe";
	std::string outputDir = "/tmp/kcov-data/";

	const char *pipePathEnv = getenv("KCOV_SYSTEM_PIPE");
	const char *outputDirEnv = getenv("KCOV_SYSTEM_DESTINATION_DIR");

	if (pipePathEnv)
	{
		destinationPipe = pipePathEnv;
	}
	if (outputDirEnv)
	{
		outputDir = outputDirEnv;
	}

	struct stat st;

	if (lstat(destinationPipe.c_str(), &st) != 0)
	{
		fprintf(stderr, "kcov-system-lib: Pipe %s doesn't exist. Giving up\n", destinationPipe.c_str());
		return;
	}


	char buf[4096];
	memset(buf, 0, sizeof(buf));
	int rv = readlink("/proc/self/exe", buf, sizeof(buf));

	if (rv < 0)
	{
		fprintf(stderr, "kcov-system-lib: Can't read /proc/self/exe link\n");
		return;
	}
	buf[sizeof(buf) - 1] = '\0';
	std::string filename = buf;


	// Used to make sure the process waits here until the daemon has attached
	std::string readbackPipe = fmt("%s/%d", outputDir.c_str(), getpid());

	if (::mkfifo(readbackPipe.c_str(), 0644) < 0)
	{
		fprintf(stderr, "kcov-system-lib: Can't create readback pipe\n");
		return;
	}

	struct new_process_entry *pe = createProcessEntry(getpid(), filename);

	if (write_file(pe, pe->entry_size, "%s", destinationPipe.c_str()) >= 0)
	{
		int fd = open(readbackPipe.c_str(), O_RDONLY);
		if (fd < 0)
		{
			unlink(readbackPipe.c_str());
			return;
		}

		// Block until something exists
		rv = read(fd, buf, sizeof(buf));

		if (rv < 0)
		{
			fprintf(stderr, "kcov-system-lib: Warning: Read-back returned error?\n");
		}

		close(fd);
		unlink(readbackPipe.c_str());
	}
	free((void *)pe);
}
