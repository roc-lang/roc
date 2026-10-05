#include <stdint.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <functional>

struct header;

namespace kcov
{
	/**
	 * Combine basic block/file info into an "address"
	 *
	 * @param filename the source filename
	 * @param function the function to map for
	 * @param basicBlock the basic block number
	 * @param lineIndex the index number in the basic block map
	 */
	static inline uint64_t gcovGetAddress(const std::string &filename, int32_t function,
			int32_t basicBlock, int32_t lineIndex)
	{
		uint32_t fn16 = ((uint32_t)function) & 0xffff;
		uint32_t bb16 = ((uint32_t)basicBlock) & 0xffff;
		uint64_t fnAndBb = (fn16 << 16) | bb16;
		uint64_t fileNameHash = (((uint64_t)std::hash<std::string>()(filename)) & 0xffffff00ULL);
		uint64_t lineIndexMask = lineIndex & 0xffULL;

		return (fileNameHash << 32ULL) | (lineIndexMask << 32ULL) | fnAndBb;
	}

	class GcovParser
	{
	public:
		/**
		 * Parse the gcov file.
		 *
		 * @return true if the file was OK, false otherwise
		 */
		bool parse();

	protected:
		GcovParser(const uint8_t *data, size_t dataSize);

		virtual ~GcovParser();

		virtual bool onRecord(const struct header *header, const uint8_t *data) = 0;

		bool verify();

		const uint8_t *readString(const uint8_t *p, std::string &out);

		// Convenience when using 32-bit pointers
		const int32_t *readString(const int32_t *p, std::string &out);


		const uint8_t *padPointer(const uint8_t *p);

	private:
		const uint8_t *m_data;
		size_t m_dataSize;
	};

	// Specialized parser for gcno files
	class GcnoParser : public GcovParser
	{
	public:
		// Holder-class for fn/bb -> file/line
		class BasicBlockMapping
		{
		public:
			int32_t m_function;
			int32_t m_basicBlock;
			std::string m_file;
			int32_t m_line;
			int32_t m_index;

			BasicBlockMapping(const BasicBlockMapping &other);

			BasicBlockMapping(int32_t function, int32_t basicBlock,
					const std::string &file, int32_t line, int32_t index);
		};

		// Holder-class for arcs between blocks
		class Arc
		{
		public:
			int32_t m_function;
			int32_t m_srcBlock;
			int32_t m_dstBlock;

			Arc(const Arc &other);

			Arc(int32_t function, int32_t srcBlock, int32_t dstBlock);
		};

		typedef std::vector<BasicBlockMapping> BasicBlockList_t;
		typedef std::vector<int32_t> FunctionList_t;
		typedef std::vector<Arc> ArcList_t;



		GcnoParser(const uint8_t *data, size_t dataSize);

		/* Return a reference to the basic blocks to file/line in the file.
		 * Empty if parse() hasn't been called.
		 *
		 * @return the basic blocks
		 */
		const BasicBlockList_t &getBasicBlocks();

		/**
		 * Return a reference to the arcs in the file. Empty if parse() hasn't
		 * been called.
		 *
		 * @return the arcs
		 */
		const ArcList_t &getArcs();

		/**
		 * Return a reference to the function IDs in the file. Empty if parse()
		 * hasn't been called.
		 *
		 * @return the functions
		 */
		const FunctionList_t &getFunctions();

	protected:
		bool onRecord(const struct header *header, const uint8_t *data);

	private:
		// Handler for record types
		void onAnnounceFunction(const struct header *header, const uint8_t *data);
		void onBlocks(const struct header *header, const uint8_t *data);
		void onLines(const struct header *header, const uint8_t *data);
		void onArcs(const struct header *header, const uint8_t *data);

		std::string m_file;
		std::string m_function;
		int32_t m_functionId;
		FunctionList_t m_functions;
		BasicBlockList_t m_basicBlocks;
		ArcList_t m_arcs;
	};

	class GcdaParser : public GcovParser
	{
	public:
		GcdaParser(const uint8_t *data, size_t dataSize);

		/**
		 * Return the number of counters for a particular function.
		 *
		 * @param function the function
		 *
		 * @return the number of counters, or 0 for invalid functions
		 */
		size_t countersForFunction(int32_t function);

		/**
		 * Return the counter value for a function.
		 *
		 * @param function the function
		 * @param counter the counter number
		 *
		 * @return the counter value, or -1 for invalid function/counters
		 */
		int64_t getCounter(int32_t function, int32_t counter);

	protected:
		bool onRecord(const struct header *header, const uint8_t *data);

		void onAnnounceFunction(const struct header *header, const uint8_t *data);

		void onCounterBase(const struct header *header, const uint8_t *data);

		typedef std::vector<int64_t> CounterList_t;
		typedef std::unordered_map<int32_t, CounterList_t> FunctionToCountersMap_t;

		int32_t m_functionId;
		FunctionToCountersMap_t m_functionToCounters;
	};
}
