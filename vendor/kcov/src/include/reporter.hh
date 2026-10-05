#pragma once

#include <string>

#include <stddef.h>
#include <stdint.h>

namespace kcov
{
	class IFileParser;
	class ICollector;
	class IFilter;

	/**
	 * Interface class that reports addresses pairs being executed.
	 *
	 * Can also be queried for code lines etc.
	 */
	class IReporter
	{
	public:
		class LineExecutionCount
		{
		public:
			LineExecutionCount(unsigned int hits, unsigned int possibleHits, uint64_t order) :
				m_hits(hits), m_possibleHits(possibleHits), m_order(order)
			{
			}

			unsigned int m_hits;
			unsigned int m_possibleHits;
			uint64_t m_order;
		};

		class ExecutionSummary
		{
		public:
			ExecutionSummary() : m_lines(0), m_executedLines(0), m_includeInTotals(true)
			{
			}

			ExecutionSummary(unsigned int lines, unsigned int executedLines) :
				m_lines(lines), m_executedLines(executedLines), m_includeInTotals(true)
			{
			}

			unsigned int m_lines;
			unsigned int m_executedLines;
			unsigned int m_includeInTotals;
		};

		/**
		 * Listener to executed addresses.
		 */
		class IListener
		{
		public:
			/**
			 * An address has been executed by the covered program. Can also
			 * report data from previous runs.
			 *
			 * @param addr the executed address
			 * @param hits the number of hits of the address (typically 1)
			 */
			virtual void onAddress(uint64_t addr, unsigned long hits) = 0;

			/**
			 * Re-report on-lines from the file-parser.
			 *
			 * The address can be changed by the reporter, so this will match
			 * onAddress above.
			 *
			 * @param file the source file
			 * @param lineNr the line number in @a file
			 * @param addr the (hashed) address for this file/line combination
			 */
			virtual void onLineReporter(const std::string &file, unsigned int lineNr, uint64_t addr) {}
		};

		virtual ~IReporter() {}


		/**
		 * Register a listener for reported addresses.
		 *
		 * @param listener the listener
		 */
		virtual void registerListener(IListener &listener) = 0;

		/**
		 * Return if a file path should be included in the output.
		 *
		 * @param file the file path to check
		 *
		 * @return true if the file should be included in the output
		 */
		virtual bool fileIsIncluded(const std::string &file) = 0;

		/**
		 * Returns if a file/line pair contains executable code.
		 *
		 * @param file the filename
		 * @param lineNr the line number in the file
		 *
		 * @return true if this is executable code, false otherwise
		 */
		virtual bool lineIsCode(const std::string &file, unsigned int lineNr) = 0;

		/**
		 * Get the execution count for a file:line pair
		 *
		 * @param file the filename to check
		 * @param lineNr the line to check
		 *
		 * @return the execution count
		 */
		virtual LineExecutionCount getLineExecutionCount(const std::string &file, unsigned int lineNr) = 0;

		/**
		 * Get a summary of what has been executed so far
		 *
		 * @return the summary
		 */
		virtual ExecutionSummary getExecutionSummary() = 0;

		virtual void writeCoverageDatabase() = 0;

		static IReporter &create(IFileParser &elf, ICollector &collector, IFilter &filter);
		static IReporter &createDummyReporter();
	};
}
