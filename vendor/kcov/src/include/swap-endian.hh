#pragma once

#include <stdlib.h>
#include <stdint.h>

// From http://stackoverflow.com/questions/105252/how-do-i-convert-between-big-endian-and-little-endian-values-in-c
template <typename T>
T swap_endian(T u)
{
	union
	{
		T u;
		unsigned char u8[sizeof(T)];
	} source, dest;

	source.u = u;

	for (size_t k = 0; k < sizeof(T); k++)
		dest.u8[k] = source.u8[sizeof(T) - k - 1];

	return dest.u;
}


static inline bool cpu_is_little_endian()
{
	static const uint16_t data = 0x1122;
	const uint8_t *p = (uint8_t *)&data;

	return p[0] == 0x22;
}

template <typename T>
T to_be(T u)
{
	if (cpu_is_little_endian())
		return swap_endian<T>(u);
	else
		return u;
}

template <typename T>
T to_le(T u)
{
	if (cpu_is_little_endian())
		return u;
	else
		return swap_endian<T>(u);
}

template <typename T>
T be_to_host(T u)
{
	if (cpu_is_little_endian())
		return swap_endian<T>(u);
	else
		return u;
}

template <typename T>
T le_to_host(T u)
{
	if (cpu_is_little_endian())
		return u;
	else
		return swap_endian<T>(u);
}
