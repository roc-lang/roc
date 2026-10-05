#include "dwarf.hh"

#include <utils.hh>

#include <elfutils/libdw.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>


using namespace kcov;

class DwarfParser::Impl
{
public:
	Impl() :
	m_fd(-1),
	m_dwarf(NULL)
	{
	}

	int m_fd;
	Dwarf *m_dwarf;
};

DwarfParser::DwarfParser()
{
	m_impl = new DwarfParser::Impl();
}

DwarfParser::~DwarfParser()
{
	close();
	delete m_impl;
}

void DwarfParser::forEachLine(IFileParser::ILineListener& listener)
{
	if (!m_impl->m_dwarf)
		return;

	Dwarf_Off offset = 0;
	Dwarf_Off lastOffset = 0;
	size_t headerSize;

	/* Iterate over the headers */
	while (dwarf_nextcu(m_impl->m_dwarf, offset, &offset, &headerSize, 0, 0, 0) == 0)
	{
		Dwarf_Lines* lines;
		Dwarf_Files *files;
		size_t lineCount;
		size_t fileCount;
		Dwarf_Die die;
		unsigned int i;

		if (dwarf_offdie(m_impl->m_dwarf, lastOffset + headerSize, &die) == NULL)
			continue;

		lastOffset = offset;

		/* Get the source lines */
		if (dwarf_getsrclines(&die, &lines, &lineCount) != 0)
			continue;

		/* And the files */
		if (dwarf_getsrcfiles(&die, &files, &fileCount) != 0)
			continue;

		const char *const *srcDirs;
		size_t ndirs = 0;

		/* Lookup the compilation path */
		if (dwarf_getsrcdirs(files, &srcDirs, &ndirs) != 0)
			continue;

		if (ndirs == 0)
			continue;

		/* Iterate through the source lines */
		for (i = 0; i < lineCount; i++)
		{
			Dwarf_Line *line;
			int lineNr = 0;
			const char* lineSource;
			Dwarf_Word mtime, len;
			bool isCode;
			Dwarf_Addr addr;

			if ( !(line = dwarf_onesrcline(lines, i)) )
				continue;

			if (dwarf_lineno(line, &lineNr) != 0)
				continue;

			if (!(lineSource = dwarf_linesrc(line, &mtime, &len)) )
				continue;

			if (dwarf_linebeginstatement(line, &isCode) != 0)
				continue;

			if (dwarf_lineaddr(line, &addr) != 0)
				continue;

			// Invalid line number?
			if (lineNr == 0)
				continue;

			// Non-code?
			if (!isCode)
				continue;

			listener.onLine(fullPath(srcDirs, lineSource), lineNr, addr);
		}
	}
}

void DwarfParser::forAddress(IFileParser::ILineListener& listener, uint64_t address)
{
	if (!m_impl->m_dwarf)
		return;

	Dwarf_Off offset = 0;
	Dwarf_Off lastOffset = 0;
	size_t headerSize;

	if (!m_impl->m_dwarf)
		return;

	/* Iterate over the headers */
	while (dwarf_nextcu(m_impl->m_dwarf, offset, &offset, &headerSize, 0, 0, 0) == 0)
	{
		Dwarf_Die die;

		if (dwarf_offdie(m_impl->m_dwarf, lastOffset + headerSize, &die) == NULL)
		{
			lastOffset = offset;
			continue;
		}

		lastOffset = offset;

		Dwarf_Line *line;
		Dwarf_Files *files;
		size_t fileCount;

		line = dwarf_getsrc_die(&die, address);
		if (!line)
			continue;

		/* And the files */
		if (dwarf_getsrcfiles(&die, &files, &fileCount) != 0)
			continue;

		const char* lineSource;
		Dwarf_Word mtime, len;

		if (!(lineSource = dwarf_linesrc(line, &mtime, &len)) )
			return;

		const char *const *srcDirs;
		size_t ndirs = 0;

		/* Lookup the compilation path */
		if (dwarf_getsrcdirs(files, &srcDirs, &ndirs) != 0)
			continue;

		int lineNr;

		if (dwarf_lineno(line, &lineNr) != 0)
			continue;

		listener.onLine(fullPath(srcDirs, lineSource), lineNr, address);
	}
}


std::string DwarfParser::fullPath(const char *const *srcDirs, const std::string &filename)
{
	/* Use the full compilation path unless the source already
	 * has an absolute path */
	std::string fullFilePath;
	std::string filePath = filename;

	std::string dir = srcDirs[0] == NULL ? "" : srcDirs[0];
	fullFilePath = dir_concat(dir, filename);
	if (filename[0] != '/')
		filePath = fullFilePath;

	return filePath;
}



bool DwarfParser::open(const std::string& filename)
{
	close();

	m_impl->m_fd = ::open(filename.c_str(), O_RDONLY);

	if (m_impl->m_fd < 0)
		return false;

	/* Initialize libdwarf from the file descriptor */
	m_impl->m_dwarf = dwarf_begin(m_impl->m_fd, DWARF_C_READ);

	if (!m_impl->m_dwarf)
	{
		::close(m_impl->m_fd);
		m_impl->m_fd = -1;

		return false;
	}

	return true;
}

void DwarfParser::close()
{
	if (m_impl->m_dwarf)
		dwarf_end(m_impl->m_dwarf);

	if (m_impl->m_fd >= 0)
		::close(m_impl->m_fd);

	m_impl->m_fd = -1;
	m_impl->m_dwarf = NULL;
}
