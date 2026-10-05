#pragma once

#include <string>

namespace kcov
{
	/**
	 * Singleton class for kcov capabilities, typically for different engines.
	 */
	class ICapabilities
	{
	public:
		virtual ~ICapabilities()
		{
		}

		/**
		 * Add a kcov capability.
		 *
		 * Will panic if an unknown capability is added.
		 *
		 * @param name the name of the capability
		 */
		virtual void addCapability(const std::string &name) = 0;

		/**
		 * Remove a kcov capability.
		 *
		 * Will panic if an unknown capability is removed.
		 *
		 * @param name the name of the capability
		 */
		virtual void removeCapability(const std::string &name) = 0;

		/**
		 * Return if kcov has a capability.
		 *
		 * Will panic if an unknown capability is added.
		 *
		 * @param name the name of the capability.
		 */
		virtual bool hasCapability(const std::string &name) = 0;


		/**
		 * Singleton getter.
		 *
		 * @return a reference to the capability singleton
		 */
		static ICapabilities &getInstance();
	};
}
