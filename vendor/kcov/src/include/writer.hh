#pragma once

namespace kcov
{
	class IFileParser;
	class IReporter;

	/**
	 * Class that writes output (HTML, Cobertura, ...)
	 */
	class IWriter
	{
	public:
		virtual ~IWriter() {}

		/**
		 * Called once on startup to setup the writer.
		 */
		virtual void onStartup() = 0;

		/**
		 * Called once on termination to shutdown the writer.
		 */
		virtual void onStop() = 0;

		/**
		 * Write current data.
		 *
		 * Called in regular intervals during execution.
		 */
		virtual void write() = 0;
	};
}
