
#include <sched.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <unistd.h>
#include <limits.h>
#include <time.h>

#include <utils.hh>

#include <sstream>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <unordered_map>

#include <curl/curl.h>


int g_kcov_debug_mask = STATUS_MSG;
static void* (*mocked_read_callback)(size_t* out_size, const char* path);
static int (*mocked_write_callback)(const void* data, size_t size, const char* path);
static bool (*mocked_file_exists_callback)(const std::string &path);
static uint64_t (*mocked_get_file_timestamp_callback)(const std::string &path);

void msleep(uint64_t ms)
{
	struct timespec ts;
	uint64_t ns = ms * 1000 * 1000;

	ts.tv_sec = ns / (1000 * 1000 * 1000);
	ts.tv_nsec = ns % (1000 * 1000 * 1000);

	nanosleep(&ts, NULL);
}

static void *read_file_int(size_t *out_size, uint64_t timeout, const char *path)
{
	uint8_t *data = NULL;
	int fd;
	size_t pos = 0;
	const size_t chunk = 1024;
	fd_set rfds;
	struct timeval tv;
	int ret;
	int n;

	if (mocked_read_callback)
		return mocked_read_callback(out_size, path);

	fd = open(path, O_RDONLY | O_NONBLOCK);
	if (fd < 0 && (errno == ENXIO || errno == EWOULDBLOCK))
	{
		msleep(timeout);

		fd = open(path, O_RDONLY | O_NONBLOCK);
	}

	if (fd < 0)
		return NULL;

	tv.tv_sec = timeout / 1000;
	tv.tv_usec = (timeout % 1000) * 10;

	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);

	do
	{
		ret = select(fd + 1, &rfds, NULL, NULL, &tv);
		if (ret == -1)
		{
			close(fd);
			free(data);

			return NULL;
		}
		else if (ret == 0)
		{ // Timeout
			close(fd);
			free(data);

			return NULL;
		}
		data = (uint8_t *)xrealloc(data, pos + chunk);
		memset(data + pos, 0, chunk);

		n = read(fd, data + pos, chunk);
		if (n < 0)
		{
			close(fd);
			free(data);

			return NULL;
		}

		pos += n;
	} while (n == (int)chunk);

	*out_size = pos;

	close(fd);

	return data;
}

void *read_file(size_t *out_size, const char *fmt, ...)
{
	char path[2048];
	va_list ap;
	int r;

	/* Create the filename */
	va_start(ap, fmt);
	r = vsnprintf(path, 2048, fmt, ap);
	va_end(ap);

	panic_if (r >= 2048,
			"Too long string!");

	return read_file_int(out_size, 0, path);
}

void *peek_file(size_t *out_size, const char *fmt, ...)
{
	char path[2048];
	va_list ap;
	int r;

	/* Create the filename */
	va_start(ap, fmt);
	r = vsnprintf(path, 2048, fmt, ap);
	va_end(ap);

	panic_if (r >= 2048,
			"Too long string!");

	/* Get the file status */
	struct stat st;
	if (lstat(path, &st) < 0)
		return NULL;
	// Regular file?
	if (S_ISREG(st.st_mode) == 0)
		return NULL;

	// Read a little bit of the file    
	const size_t to_read = 512;    
	char *out = static_cast<char*>(xmalloc(to_read));
    
	std::ifstream str(path);
	if (str.is_open())
	{
		str.read(out,to_read);
		// If successful (all desired characters) or
		// a non-zero number of characters were read, then succeed
		if (str || str.gcount() > 0)
		{
			*out_size = str.gcount();
		}
		else
		{
			// No characters read, so fail
			free(out);
			out = NULL;
		}
		str.close();

	}
	else
	{
		free(out);
		out = NULL;
	}

	return out;
}

static int write_file_int(const void *data, size_t len, uint64_t timeout, const char *path)
{
	int fd;
	fd_set wfds;
	struct timeval tv;
	int ret = 0;

	tv.tv_sec = timeout / 1000;
	tv.tv_usec = (timeout % 1000) * 10;

	fd = open(path, O_WRONLY | O_CREAT | O_TRUNC | O_NONBLOCK, S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH);
	if (fd < 0 && (errno == ENXIO || errno == EWOULDBLOCK))
	{
		msleep(timeout);

		fd = open(path, O_WRONLY | O_CREAT | O_TRUNC | O_NONBLOCK, S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH);
		if (fd < 0 && (errno == ENXIO || errno == EWOULDBLOCK))
			return -2;
	}
	if (fd < 0)
		return fd;

	FD_ZERO(&wfds);
	FD_SET(fd, &wfds);

	ret = select(fd + 1, NULL, &wfds, NULL, &tv);
	if (ret == -1)
	{
		close(fd);

		return ret;
	}
	else if (ret == 0)
	{
		// Timeout
		close(fd);

		return -2;
	}

	if (write(fd, data, len) != (int)len)
		return -3;

	close(fd);

	return 0;
}

int write_file(const void *data, size_t len, const char *fmt, ...)
{
	char path[2048];
	va_list ap;

	/* Create the filename */
	va_start(ap, fmt);
	vsnprintf(path, 2048, fmt, ap);
	va_end(ap);

	if (mocked_write_callback)
		return mocked_write_callback(data, len, path);

	return write_file_int(data, len, 0, path);
}


std::string dir_concat(const std::string &dirIn, const std::string &filenameIn)
{
	if (dirIn == "")
		return filenameIn;

	std::string dir = dirIn;
	std::string filename = filenameIn;

	// Slash extra slashses
	while (dir[dir.size() - 1] == '/')
	{
		dir = dir.substr(0, dir.size() - 1);
	}

	while (filename[0] == '/')
	{
		filename = filename.substr(1);
	}

	return dir + "/" + filename;
}

const char *get_home(void)
{
	return getenv("HOME");
}

bool make_file_non_blocking(FILE *fp)
{
	int fd = fileno(fp);
	int flags = fcntl(fd, F_GETFL, 0);
	if (flags == -1)
		return false;
	else
		return fcntl(fd, F_SETFL, flags | O_NONBLOCK) != -1;
}

bool file_readable(FILE *fp, unsigned int ms)
{
	fd_set rfds;
	struct timeval tv;
	int rv;

	int fd = fileno(fp);

	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);

	tv.tv_sec = ms / 1000;
	tv.tv_usec = (ms % 1000) * 1000;

	rv = select(fd + 1, &rfds, NULL, NULL, &tv);

	return rv > 0;
}

static std::unordered_map<std::string, bool> statCache;

bool file_exists(const std::string &path)
{
	if (mocked_file_exists_callback)
		return mocked_file_exists_callback(path);

	bool out;

	if (statCache.find(path) == statCache.end())
	{
		struct stat st;

		out = lstat(path.c_str(), &st) == 0;

		statCache[path] = out;
	}
	else
	{
		out = statCache[path];
	}

	return out;
}

bool executable_exists_in_path(const std::string &executableName)
{
	// Full path to it
	if (file_exists(executableName))
	{
		return true;
	}

	std::string path = getenv("PATH");
	std::vector<std::string> parts = split_string(path, ":");

	for (std::vector<std::string>::iterator it = parts.begin();
			it != parts.end();
			++it)
	{
		std::string cur = fmt("%s/%s", it->c_str(), executableName.c_str());

		if (file_exists(cur))
		{
			return true;
		}
	}

	return false;
}

void mock_read_file(void *(*callback)(size_t *out_size, const char *path))
{
	mocked_read_callback = callback;
}

void mock_write_file(int (*callback)(const void *data, size_t size, const char *path))
{
	mocked_write_callback = callback;
}

void mock_file_exists(bool (*callback)(const std::string &path))
{
	mocked_file_exists_callback = callback;
}


void mock_get_file_timestamp(uint64_t (*callback)(const std::string &path))
{
	mocked_get_file_timestamp_callback = callback;
}

uint64_t get_file_timestamp(const std::string &path)
{
	if (mocked_get_file_timestamp_callback)
		return mocked_get_file_timestamp_callback(path);

	struct stat st;

	// "Fail"
	if (lstat(path.c_str(), &st) != 0)
		return 0;

	// See http://stackoverflow.com/questions/11373505/getting-the-last-modified-date-of-a-file-in-c
#ifdef __APPLE__
#ifndef st_mtim
#define st_mtim st_mtimespec
#endif
#endif

    return st.st_mtim.tv_sec;
}

static void read_write(FILE *dst, FILE *src)
{
	char buf[1024];

	while (!feof(src))
	{
		int n = fread(buf, sizeof(char), sizeof(buf), src);

		fwrite(buf, sizeof(char), n, dst);
	}
}

int concat_files(const char *dst_name, const char *file_a, const char *file_b)
{
	FILE *dst;
	FILE *s1, *s2;
	int ret = -1;

	dst = fopen(dst_name, "w");
	if (!dst)
		return -1;
	s1 = fopen(file_a, "r");
	if (!s1)
		goto out_dst;
	s2 = fopen(file_b, "r");
	if (!s2)
		goto out_s1;

	read_write(dst, s1);
	read_write(dst, s2);

	fclose(s2);
out_s1:
	fclose(s1);
out_dst:
	fclose(dst);

	return ret;
}

unsigned long get_aligned(unsigned long addr)
{
	return (addr / sizeof(long)) * sizeof(long);
}

unsigned long get_aligned_4b(unsigned long addr)
{
	return addr & ~3;
}

std::string fmt(const char *fmt, ...)
{
	char buf[4096];
	va_list ap;
	int res;

	va_start(ap, fmt);
	res = vsnprintf(buf, sizeof(buf), fmt, ap);
	va_end(ap);

	if (res >= (int)sizeof(buf))
	{
		std::string result;
		result.resize(res+1);

		va_start(ap, fmt);
		res = vsnprintf(&result[0], result.size(), fmt, ap);
		va_end(ap);

		panic_if(res >= (int)result.size(),
				"Buffer overflow");

		result.resize(res);
		return result;
	}

	return std::string(buf);
}

void mdelay(unsigned int ms)
{
	struct timespec ts;
	ts.tv_sec = 0;
	ts.tv_nsec = ms * 1000 * 1000;

	nanosleep(&ts, NULL);
}

uint64_t get_ms_timestamp(void)
{
	static uint64_t first;
	struct timeval tv;

	gettimeofday(&tv, NULL);

	if (first == 0)
		first = (tv.tv_sec * 1000000ULL + tv.tv_usec) / 1000;

	return ((tv.tv_sec * 1000000ULL + tv.tv_usec) / 1000) - first;
}

bool machine_is_64bit(void)
{
	return sizeof(unsigned long) == 8;
}


// http://stackoverflow.com/questions/236129/how-to-split-a-string-in-c
static std::vector<std::string> &split(const std::string &s, char delim,
		std::vector<std::string> &elems)
{
    std::stringstream ss(s);
    std::string item;

    while (std::getline(ss, item, delim))
    {
        elems.push_back(item);
    }

    return elems;
}


std::vector<std::string> split_string(const std::string &s, const char *delims)
{
    std::vector<std::string> elems;
    split(s, *delims, elems);

    return elems;
}

std::string escape_url(const std::string &s)
{
	std::vector<std::string> elems = split_string(s, "/");
	std::string out;

	// Yes, inefficient, but the number of invocations is fairly limited in kcov
	CURL *curl = curl_easy_init();
	std::string delim = "";

	for (std::vector<std::string>::iterator it = elems.begin(); it != elems.end(); ++it)
	{
		char *p = curl_easy_escape(curl, it->c_str(), 0);
		std::string cur = *it;

		if (p)
		{
			cur = p;
		}
		out = out + delim + cur;
		delim = "/";
		free(p);
	}
	curl_easy_cleanup(curl);

	return out;
}

std::string trim_string(const std::string &strIn, const std::string &trimEndChars)
{
	std::string str = strIn;
	size_t endpos = str.find_last_not_of(trimEndChars);

	if (std::string::npos != endpos)
		str = str.substr( 0, endpos+1 );

	// trim leading spaces
	size_t startpos = str.find_first_not_of(" \t");
	if (std::string::npos != startpos)
		str = str.substr( startpos );
	else
		str = "";

	return str;
}

// Cache for ::realpath - it's apparently one of the reasons why kcov is slow
typedef std::unordered_map<std::string, std::string> PathMap_t;
static PathMap_t realPathCache;
const std::string &get_real_path(const std::string &path)
{
	PathMap_t::const_iterator it = realPathCache.find(path);
	if (it != realPathCache.end())
		return it->second;

	char *rp = NULL;

	rp = ::realpath(path.c_str(), NULL);

	if (!rp)
		return path;

	realPathCache[path] = rp;
	free(rp);

	return realPathCache[path];
}


bool string_is_integer(const std::string &str, unsigned base)
{
	size_t pos;

	try
	{
		stoull(str, &pos, base);
	}
	catch(std::invalid_argument &e)
	{
		return false;
	}
	catch(std::out_of_range &e)
	{
		return false;
	}

	return pos == str.size();
}

int64_t string_to_integer(const std::string &str, unsigned base)
{
	size_t pos;

	return (int64_t)stoull(str, &pos, base);
}



static char *escape_helper(char *dst, const char *what)
{
	int len = strlen(what);

	strcpy(dst, what);

	return dst + len;
}

std::string escape_html(const std::string &str)
{
	const char *s = str.c_str();
	char buf[4096];
	char *dst = buf;
	size_t len = strlen(s);
	bool truncated = false;
	size_t i;

	// Truncate long lines (or entries)
	if (len > 512)
	{
		len = 512;
		truncated = true;
	}

	memset(buf, 0, sizeof(buf));
	for (i = 0; i < len; i++)
	{
		char c = s[i];

		switch (c)
		{
		case '<':
			dst = escape_helper(dst, "&lt;");
			break;
		case '>':
			dst = escape_helper(dst, "&gt;");
			break;
		case '&':
			dst = escape_helper(dst, "&amp;");
			break;
		case '\"':
			dst = escape_helper(dst, "&quot;");
			break;
		case '\'':
			dst = escape_helper(dst, "&#039;");
			break;
		case '/':
			dst = escape_helper(dst, "&#047;");
			break;
		case '\\':
			dst = escape_helper(dst, "&#092;");
			break;
		case '\n': case '\r':
			dst = escape_helper(dst, " ");
			break;
		default:
			*dst = c;
			dst++;
			break;
		}
	}

	if (truncated)
		return std::string(buf) + "...";

	return std::string(buf);
}

std::string escape_json(const std::string &str)
{
	const std::string escapeChars = "\"\\\t\015'";
	size_t n_escapes = 0;

	for (unsigned i = 0; i < str.size(); i++)
	{
		for (unsigned int n = 0; n < escapeChars.size(); n++)
		{
			if (str[i] == escapeChars[n])
				n_escapes++;
		}
	}

	if (n_escapes == 0)
		return str;

	std::string out;
	out.resize(str.size() + n_escapes);
	unsigned cur = 0;

	for (unsigned i = 0; i < str.size(); i++)
	{
		out[cur] = str[i];

		// Special-case tabs
		if (str[i] == '\t')
		{
			out[cur] = '\\';
			cur++;
			out[cur] = 't';
			cur++;
			continue;
		}
		// Quote quotes and backslashes
		for (unsigned int n = 0; n < escapeChars.size(); n++)
		{
			if (str[i] == escapeChars[n])
			{
				out[cur] = '\\';
				cur++;
				out[cur] = str[i];
			}
		}
		cur++;
	}

	return out;
}

std::pair<std::string, std::string> split_path(const std::string &pathStr)
{
	std::pair<std::string, std::string> out;
	std::string path(pathStr);

	size_t pos = path.rfind('/');

	out.first = "";
	out.second = path;
	if (pos != std::string::npos)
	{
		out.first= path.substr(0, pos + 1);
		out.second = path.substr(pos + 1, std::string::npos);
	}

	return out;
}

uint32_t hash_file(const std::string &filename)
{
	size_t sz;
	void *p = read_file(&sz, "%s", filename.c_str());
	if (!p)
	{
		return 0;
	}

	uint32_t out = hash_block(p, sz);
	free(p);

	return out;
}
