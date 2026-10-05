#include "dwarf.hh"

#include <utils.hh>

#include <dwarf.h>
#include <libdwarf/libdwarf.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>


using namespace kcov;

static void dwarf_error_handler(Dwarf_Error error, Dwarf_Ptr userData)
{
	char *msg = dwarf_errmsg(error);

	printf("Dwarf: %s\n", msg);
}

class DwarfParser::Impl
{
public:
	Impl() :
	m_fd(-1),
	m_dwarf(NULL)
	{
	}

	int m_fd;
	Dwarf_Debug m_dwarf;
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
	Dwarf_Unsigned header;

	/* Iterate over the headers */
	while (dwarf_next_cu_header(m_impl->m_dwarf, 0, 0, 0, 0, &header, 0) == DW_DLV_OK)
	{
		Dwarf_Line* line_buffer;
		Dwarf_Signed line_count;
		Dwarf_Die die;
		int i;

		if (dwarf_siblingof(m_impl->m_dwarf, 0, &die, 0) != DW_DLV_OK)
			continue;

		/* Get the source lines */
		if (dwarf_srclines(die, &line_buffer, &line_count, 0) != DW_DLV_OK)
			continue;

		/* Store them */
		for (i = 0; i < line_count; i++)
		{
			Dwarf_Unsigned line_nr;
			char* line_source;
			Dwarf_Bool is_code;
			Dwarf_Addr addr;


			if (dwarf_lineno(line_buffer[i], &line_nr, 0) != DW_DLV_OK)
				continue;
			if (dwarf_linesrc(line_buffer[i], &line_source, 0) != DW_DLV_OK)
				continue;
			if (dwarf_linebeginstatement(line_buffer[i], &is_code, 0) != DW_DLV_OK)
				continue;

			if (dwarf_lineaddr(line_buffer[i], &addr, 0) != DW_DLV_OK)
				continue;

			if (line_nr && is_code)
			{
				static char *srcDirs[1] = {NULL};
				listener.onLine(fullPath(srcDirs, line_source), line_nr, addr);
			}

			dwarf_dealloc(m_impl->m_dwarf, line_source, DW_DLA_STRING);
		}
	}
}

void DwarfParser::forAddress(IFileParser::ILineListener& listener, uint64_t address)
{
	panic("NYI");
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

	/* Initialize libdwarf */
	int err = dwarf_init(m_impl->m_fd, DW_DLC_READ, dwarf_error_handler, 0, &m_impl->m_dwarf,0);
	if (err == DW_DLV_ERROR)
	{
		::close(m_impl->m_fd);
		m_impl->m_fd = -1;

		return false;
	}

	return true;
}

void DwarfParser::close()
{
	if (m_impl->m_fd < 0)
		return;

	dwarf_finish(m_impl->m_dwarf, 0);

	::close(m_impl->m_fd);

	m_impl->m_fd = -1;
	m_impl->m_dwarf = NULL;
}
