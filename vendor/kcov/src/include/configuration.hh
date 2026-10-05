#pragma once

#include <string>
#include <list>
#include <vector>

namespace kcov
{
	/**
	 * Class that hold kcov configuration (command line options)
	 */
	class IConfiguration
	{
	public:
		typedef enum
		{
			MODE_COLLECT_ONLY       = 1,
			MODE_REPORT_ONLY        = 2,
			MODE_COLLECT_AND_REPORT = 3,
			MODE_MERGE_ONLY         = 4,
			MODE_SYSTEM_RECORD      = 5,
			MODE_SYSTEM_REPORT      = 6,
		} RunMode_t;

		class IListener
		{
		public:
			virtual ~IListener()
			{
			}

			/**
			 * Callback for configuration changes.
			 *
			 * Listeners will receive this call on each configuration change. The
			 * call is made from a configuration thread, so watch out with races.
			 *
			 * @param key the name of the setting
			 */
			virtual void onConfigurationChanged(const std::string &key) = 0;
		};
		typedef std::vector<IListener *> ConfigurationListener_t;


		virtual ~IConfiguration() {}


		/**
		 * Print kcov usage.
		 */
		virtual void printUsage() = 0;


		/**
		 * Return a value as a string.
		 *
		 * Will panic if the key is not present.
		 *
		 * @param key the key to lookup
		 *
		 * @return the string value
		 */
		virtual const std::string &keyAsString(const std::string &key) = 0;

		/**
		 * Return a value as an integer.
		 *
		 * Will panic if the key is not present.
		 *
		 * @param key the key to lookup
		 *
		 * @return the integer value
		 */
		virtual int keyAsInt(const std::string &key) = 0;

		/**
		 * Return a value as a string list.
		 *
		 * Will panic if the key is not present.
		 *
		 * @param key the key to lookup
		 *
		 * @return the values as a list, potentially empty
		 */
		virtual const std::vector<std::string> &keyAsList(const std::string &key) = 0;

		/**
		 * Set a key. Use these with caution.
		 */
		virtual void setKey(const std::string &key, const std::string &val) = 0;

		virtual void setKey(const std::string &key, int val) = 0;

		virtual void setKey(const std::string &key, const std::vector<std::string> &val) = 0;

		/**
		 * Return the coveree argv (i.e., without kcov and kcov options)
		 *
		 * @return The coveree argv
		 */
		virtual const char **getArgv() = 0;

		/**
		 * Return the coveree argc (i.e., without kcov and kcov options)
		 *
		 * @return The coveree argc
		 */
		virtual unsigned int getArgc() = 0;

		/**
		 * Register a configuration-changed listener
		 *
		 * @param listener the listener to call on changes
		 * @param keys the keys to listen for
		 */
		virtual void registerListener(IListener &listener, const std::vector<std::string> &keys) = 0;


		/**
		 * Parse argc, argv and setup the configuration
		 *
		 * @return true if the configuration is OK
		 */
		virtual bool parse(unsigned int argc, const char *argv[]) = 0;

		static IConfiguration &getInstance();
	};
}
