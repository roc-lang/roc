#include <gcov.hh>
#include <utils.hh>

using namespace kcov;

/*
 * From gcov-io.h:
 *
 *  int32:  byte3 byte2 byte1 byte0 | byte0 byte1 byte2 byte3
 *  int64:  int32:low int32:high
 *  string: int32:0 | int32:length char* char:0 padding
 *  padding: | char:0 | char:0 char:0 | char:0 char:0 char:0
 *  item: int32 | int64 | string
 *
 * File:
 *  file : int32:magic int32:version int32:stamp record*
 *
 * Record:
 *  record: header data
 *  header: int32:tag int32:length
 *  data: item*
 *
 * gcno file records:
 *  note: unit function-graph*
 *  unit: header int32:checksum string:source
 *  function-graph: announce_function basic_blocks {arcs | lines}*
 *  announce_function: header int32:ident
 *    int32:lineno_checksum int32:cfg_checksum
 *    string:name string:source int32:lineno
 *  basic_block: header int32:flags*
 *  arcs: header int32:block_no arc*
 *  arc:  int32:dest_block int32:flags
 *  lines: header int32:block_no line*
 *    int32:0 string:NULL
 *  line:  int32:line_no | int32:0 string:filename
 *
 * gcda file records:
 *   data: {unit summary:object summary:program* function-data*}*
 *   unit: header int32:checksum
 *   function-data:	announce_function present counts
 *   announce_function: header int32:ident
 *       int32:lineno_checksum int32:cfg_checksum
 *   present: header int32:present
 *   counts: header int64:count*
 *   summary: int32:checksum {count-summary}GCOV_COUNTERS_SUMMABLE
 *   count-summary:	int32:num int32:runs int64:sum
 *      int64:max int64:sum_max histogram
 *   histogram: {int32:bitvector}8 histogram-buckets*
 *     histogram-buckets: int32:num int64:min int64:sum
 */
#define GCOV_DATA_MAGIC ((uint32_t)0x67636461) /* "gcda" */
#define GCOV_NOTE_MAGIC ((uint32_t)0x67636e6f) /* "gcno" */

#define GCOV_TAG_FUNCTION	 ((uint32_t)0x01000000)
#define GCOV_TAG_FUNCTION_LENGTH (3)
#define GCOV_TAG_BLOCKS		 ((uint32_t)0x01410000)
#define GCOV_TAG_BLOCKS_LENGTH(NUM) (NUM)
#define GCOV_TAG_BLOCKS_NUM(LENGTH) (LENGTH)
#define GCOV_TAG_ARCS		 ((uint32_t)0x01430000)
#define GCOV_TAG_ARCS_LENGTH(NUM)  (1 + (NUM) * 2)
#define GCOV_TAG_ARCS_NUM(LENGTH)  (((LENGTH) - 1) / 2)
#define GCOV_TAG_LINES		 ((uint32_t)0x01450000)
#define GCOV_TAG_COUNTER_BASE 	 ((uint32_t)0x01a10000)
#define GCOV_TAG_COUNTER_LENGTH(NUM) ((NUM) * 2)
#define GCOV_TAG_COUNTER_NUM(LENGTH) ((LENGTH) / 2)
#define GCOV_TAG_OBJECT_SUMMARY  ((uint32_t)0xa1000000) /* Obsolete */
#define GCOV_TAG_PROGRAM_SUMMARY ((uint32_t)0xa3000000)
#define GCOV_TAG_SUMMARY_LENGTH(NUM)  \
        (1 + GCOV_COUNTERS_SUMMABLE * (10 + 3 * 2) + (NUM) * 5)
#define GCOV_TAG_AFDO_FILE_NAMES ((uint32_t)0xaa000000)
#define GCOV_TAG_AFDO_FUNCTION ((uint32_t)0xac000000)
#define GCOV_TAG_AFDO_WORKING_SET ((uint32_t)0xaf000000)

/* Arc flags */
#define GCOV_ARC_ON_TREE 	 (1 << 0)
#define GCOV_ARC_FAKE		 (1 << 1)
#define GCOV_ARC_FALLTHROUGH (1 << 2)

struct file_header
{
	int32_t magic;
	int32_t version;
	int32_t stamp;
};

struct header
{
	int32_t tag;
	int32_t length;
};

GcovParser::GcovParser(const uint8_t *data, size_t dataSize) :
		m_data(data), m_dataSize(dataSize)
{
}

GcovParser::~GcovParser()
{
	free((void *) m_data);
}

bool GcovParser::parse()
{
	// Not a gcov file?
	if (!verify())
		return false;

	const uint8_t *cur = m_data + sizeof(struct file_header);
	ssize_t left = (ssize_t) (m_dataSize - sizeof(struct file_header));

	// Iterate through the headers
	while (1)
	{
		const struct header *header = (struct header *) cur;

		if (!onRecord(header, cur + sizeof(*header)))
			return false;

		size_t curLen = sizeof(struct header) + header->length * 4;
		left -= curLen;

		if (left <= 0)
			break;

		cur += curLen;
	}

	return true;
}

bool GcovParser::verify()
{
	const struct file_header *header = (const struct file_header *) m_data;

	if (header->magic != GCOV_DATA_MAGIC && header->magic != GCOV_NOTE_MAGIC)
		return false;

	return true;
}

const uint8_t *GcovParser::readString(const uint8_t *p, std::string &out)
{
	int32_t length = *(const int32_t*) p;
	const char *c_str = (const char *) &p[4];

	out = std::string(c_str);

	return padPointer(p + length * 4 + 4); // Including the length field
}

// Convenience when using 32-bit pointers
const int32_t *GcovParser::readString(const int32_t *p, std::string &out)
{
	return (const int32_t *) readString((const uint8_t *) p, out);
}

const uint8_t *GcovParser::padPointer(const uint8_t *p)
{
	unsigned long addr = (unsigned long) p;

	if ((addr & 3) != 0)
		p += 4 - (addr & 3);

	return p;
}

GcnoParser::BasicBlockMapping::BasicBlockMapping(const BasicBlockMapping &other) :
		m_function(other.m_function), m_basicBlock(other.m_basicBlock),
		m_file(other.m_file), m_line(other.m_line), m_index(other.m_index)
{
}

GcnoParser::BasicBlockMapping::BasicBlockMapping(int32_t function,
		int32_t basicBlock, const std::string &file, int32_t line,
		int32_t index) :
		m_function(function), m_basicBlock(basicBlock), m_file(file), m_line(line), m_index(index)
{
}

GcnoParser::Arc::Arc(const Arc &other) :
		m_function(other.m_function), m_srcBlock(other.m_srcBlock), m_dstBlock(
				other.m_dstBlock)
{
}

GcnoParser::Arc::Arc(int32_t function, int32_t srcBlock, int32_t dstBlock) :
		m_function(function), m_srcBlock(srcBlock), m_dstBlock(dstBlock)
{
}

GcnoParser::GcnoParser(const uint8_t *data, size_t dataSize) :
		GcovParser(data, dataSize), m_functionId(-1)
{
}

const GcnoParser::BasicBlockList_t &GcnoParser::getBasicBlocks()
{
	return m_basicBlocks;
}

const GcnoParser::FunctionList_t &GcnoParser::getFunctions()
{
	return m_functions;
}

const GcnoParser::ArcList_t &GcnoParser::getArcs()
{
	return m_arcs;
}

bool GcnoParser::onRecord(const struct header *header, const uint8_t *data)
{
	kcov_debug(ENGINE_MSG, "GCNO record: 0x%08x (%d bytes)\n", header->tag,
			header->length * 4);

	switch (header->tag)
	{
	case GCOV_TAG_FUNCTION:
		onAnnounceFunction(header, data);
		break;
	case GCOV_TAG_BLOCKS:
		onBlocks(header, data);
		break;
	case GCOV_TAG_LINES:
		onLines(header, data);
		break;
	case GCOV_TAG_ARCS:
		onArcs(header, data);
		break;
	default:
		break;
	}

	return true;
}

void GcnoParser::onAnnounceFunction(const struct header *header,
		const uint8_t *data)
{
	const int32_t *p32 = (const int32_t *) data;
	const uint8_t *p8 = data;
	int32_t ident = p32[0];

	p8 = readString(p8 + 3 * 4, m_function);
	p8 = readString(p8, m_file);
	m_functionId = ident;

	m_functions.push_back(m_functionId);

	kcov_debug(ENGINE_MSG, "GCNO function %d: %s\n", m_functionId,
			m_file.c_str());
	// The first line of this function comes next, but let's ignore that
}

void GcnoParser::onBlocks(const struct header *header, const uint8_t *data)
{
	// Not used by kcov
}

void GcnoParser::onLines(const struct header *header, const uint8_t *data)
{
	const int32_t *p32 = (const int32_t *) data;
	int32_t blockNo = p32[0];
	const int32_t *last = &p32[header->length];
	int32_t n = 0; // index

	p32++; // Skip blockNo

	// Iterate through lines
	while (p32 < last)
	{
		int32_t line = *p32;

		// File name
		if (line == 0)
		{
			std::string name;

			// Setup current file name
			p32 = readString(p32 + 1, name);
			if (name != "")
				m_file = name;

			continue;
		}

		p32++;

		kcov_debug(ENGINE_MSG, "GCNO basic block in function %d, nr %d %s:%d\n",
				m_functionId, blockNo, m_file.c_str(), line);
		m_basicBlocks.push_back(
				BasicBlockMapping(m_functionId, blockNo, m_file, line, n));

		n++;
	}
}

void GcnoParser::onArcs(const struct header *header, const uint8_t *data)
{
	const int32_t *p32 = (const int32_t *) data;
	int32_t blockNo = p32[0];
	const int32_t *last = &p32[header->length];
	unsigned int arc = 0;

	p32++; // Skip blockNo

	// Iterate through lines
	while (p32 < last)
	{
		int32_t destBlock = p32[0];
		int32_t flags = p32[1];

		// Report non-on-tree arcs
		if (!(flags & GCOV_ARC_ON_TREE))
			m_arcs.push_back(Arc(m_functionId, blockNo, destBlock));

		kcov_debug(ENGINE_MSG, "GCNO arc in function %d, %d->%d (flags %d%s)\n",
				m_functionId, blockNo, destBlock, flags,
				flags & GCOV_ARC_ON_TREE ? " OT" : "");

		p32 += 2;
		arc++;
	}
}

GcdaParser::GcdaParser(const uint8_t *data, size_t dataSize) :
		GcovParser(data, dataSize), m_functionId(-1)
{
}

size_t GcdaParser::countersForFunction(int32_t function)
{
	if (function < 0)
		panic("Garbage in!");

	if (m_functionToCounters.find(function) == m_functionToCounters.end())
		return 0;

	// Simply the size of the vector
	return m_functionToCounters[function].size();
}

int64_t GcdaParser::getCounter(int32_t function, int32_t counter)
{
	if (function < 0 || counter < 0)
		panic("Garbage in!");

	if (m_functionToCounters.find(function) == m_functionToCounters.end())
		return -1;

	// List of counters
	CounterList_t &cur = m_functionToCounters[function];
	if ((size_t) counter >= cur.size())
		return -1;

	return cur[counter];
}

bool GcdaParser::onRecord(const struct header *header, const uint8_t *data)
{
	kcov_debug(ENGINE_MSG, "GCDA record: 0x%08x (%d bytes)\n", header->tag,
			header->length * 4);

	switch (header->tag)
	{
	case GCOV_TAG_FUNCTION:
		onAnnounceFunction(header, data);
		break;
	case GCOV_TAG_COUNTER_BASE:
		onCounterBase(header, data);
		break;
	default:
		break;
	}

	return true;
}

void GcdaParser::onAnnounceFunction(const struct header *header,
		const uint8_t *data)
{
	const int32_t *p32 = (const int32_t *) data;
	int32_t ident = p32[0];

	// FIXME! Handle checksums after this
	m_functionId = ident;

	kcov_debug(ENGINE_MSG, "GCDA function %d\n", ident);
}

void GcdaParser::onCounterBase(const struct header *header, const uint8_t *data)
{
	const int32_t *p32 = (const int32_t *) data;
	int32_t count = header->length; // 32-bit data with 64-bit values

	// Store all counters in a list
	CounterList_t counters;
	for (int32_t i = 0; i < count; i += 2)
	{
		uint64_t v64 = (uint64_t) p32[i] | ((uint64_t) p32[i + 1] << 32ULL);

		counters.push_back(v64);

		kcov_debug(ENGINE_MSG, "GCDA counter %d %lld\n", i,
				(long long) counters.back());
	}

	m_functionToCounters[m_functionId] = counters;
}
