#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <semaphore.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <zlib.h>


#include <string>
#include <vector>
#include <utility>
#include <list>

#define error(x...) do \
{ \
	fprintf(stderr, "kcov: error: "); \
	fprintf(stderr, x); \
	fprintf(stderr, "\n"); \
} while(0)

#define warning(x...) do \
{ \
	fprintf(stderr, "kcov: warning: "); \
	fprintf(stderr, x); \
	fprintf(stderr, "\n"); \
} while(0)

#define panic(x...) do \
{ \
	error(x); \
	exit(1); \
} while(0)

enum debug_mask
{
	INFO_MSG   =   1,
	ENGINE_MSG =   2,
	ELF_MSG    =   4,
	BP_MSG     =   8,
	STATUS_MSG =  16,
};
extern int g_kcov_debug_mask;

static inline void kcov_debug(enum debug_mask dbg, const char *fmt, ...) __attribute__((format(printf,2,3)));

static inline void kcov_debug(enum debug_mask dbg, const char *fmt, ...)
{
	va_list ap;

	if ((g_kcov_debug_mask & dbg) == 0)
		return;

	va_start(ap, fmt);
	vfprintf(stdout, fmt, ap);
	va_end(ap);
}

#define panic_if(cond, x...) \
		do { if ((cond)) panic(x); } while(0)

static inline char *xstrdup(const char *s)
{
	char *out = strdup(s);

	panic_if(!out, "strdup failed");

	return out;
}

static inline void *xmalloc(size_t sz)
{
  void *out = malloc(sz);

  panic_if(!out, "malloc failed");
  memset(out, 0, sz);

  return out;
}

static inline void *xrealloc(void *p, size_t sz)
{
  void *out = realloc(p, sz);

  panic_if(!out, "realloc failed");

  return out;
}

extern int write_file(const void *data, size_t len, const char *fmt, ...) __attribute__((format(printf,3,4)));

extern void *read_file(size_t *out_size, const char *fmt, ...) __attribute__((format(printf,2,3)));

extern void *peek_file(size_t *out_size, const char *fmt, ...) __attribute__((format(printf,2,3)));

extern std::string dir_concat(const std::string &dir, const std::string &filename);

#define xwrite_file(data, len, dir...) do { \
	int r = write_file(data, len, dir); \
	panic_if (r != 0, "write_file failed with %d\n", r); \
} while(0)

#define xsnprintf(buf, size, fmt, x...) do { \
    int r = snprintf(buf, size, fmt, x); \
    panic_if(r < 0 || r >= (int)(size), "snprintf failed for %s with %d\n", fmt, r); \
} while(0)


extern bool file_exists(const std::string &path);

extern bool executable_exists_in_path(const std::string &path);

extern uint64_t get_file_timestamp(const std::string &path);

extern int concat_files(const char *dst, const char *file_a, const char *file_b);

extern const char *get_home();

/**
 * Make a FILE * non blocking when not enough data can be read from it
 *
 * @param fp the file to read
 *
 * @return true if the file could be made non blocking, false otherwise
 */
bool make_file_non_blocking(FILE *fp);

/**
 * Return true if a FILE * is readable without blocking.
 *
 * @param fp the file to read
 * @param ms the number of milliseconds to wait
 *
 * @return true if the file can be read without blocking, false otherwise
 */
bool file_readable(FILE *fp, unsigned int ms);

unsigned long get_aligned(unsigned long addr);

unsigned long get_aligned_4b(unsigned long addr);


std::string fmt(const char *fmt, ...) __attribute__((format(printf,1,2)));

void mdelay(unsigned int ms);

uint64_t get_ms_timestamp(void);

bool machine_is_64bit(void);

std::vector<std::string> split_string(const std::string &s, const char *delims);

std::string trim_string(const std::string &strIn, const std::string &trimEndChars = " \t\n\r");

const std::string &get_real_path(const std::string &path);

// Split into path / filename
std::pair<std::string, std::string> split_path(const std::string &pathStr);

bool string_is_integer(const std::string &str, unsigned base = 0);

int64_t string_to_integer(const std::string &str, unsigned base = 0);

std::string escape_html(const std::string &str);

std::string escape_json(const std::string &str);

std::string escape_url(const std::string &s);

void msleep(uint64_t ms);

class Semaphore
{
private:
	sem_t *m_sem;
    std::string m_name;
    
public:
	Semaphore(int initial = 1)
	{
		m_name = fmt("/kcov-sem.%p", this);

		m_sem = sem_open(m_name.c_str(), O_CREAT, 0644, initial);
        if (!m_sem)
            panic("Can't create semaphore");
	}

	~Semaphore()
	{
		sem_close(m_sem);
        sem_unlink(m_name.c_str());
	}

	void notify()
	{
		sem_post(m_sem);
	}

	void wait()
	{
		sem_wait(m_sem);
	}
};

// Unit test stuff
void mock_read_file(void *(*callback)(size_t *out_size, const char *path));

void mock_write_file(int (*callback)(const void *data, size_t size, const char *path));

void mock_file_exists(bool (*callback)(const std::string &path));

void mock_get_file_timestamp(uint64_t (*callback)(const std::string &path));


static inline uint32_t hash_block(const void *buf, size_t len)
{
	return crc32(0, (const Bytef *)buf, len);
}

uint32_t hash_file(const std::string &filename);
