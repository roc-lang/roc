#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

void __attribute__((constructor)) tracefd_cloexec(void)
{
	int xtraceFd;
	const char* fdStr;
	char* endStr;

	if (getenv("KCOV_BASH_USE_DEBUG_TRAP"))
		fdStr = getenv("KCOV_BASH_XTRACEFD");
	else
		fdStr = getenv("BASH_XTRACEFD");

	if (fdStr)
	{
		xtraceFd = (int)strtol(fdStr, &endStr, 10);
		if (endStr != NULL && *endStr == '\0')
		{
			int flags = fcntl(xtraceFd, F_GETFD);
			if (flags != -1)
				fcntl(xtraceFd, F_SETFD, flags | FD_CLOEXEC);
		}
	}
}
