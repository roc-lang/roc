#ifndef _NOCOVER_H_
#define _NOCOVER_H_

#include <string>
namespace kcov
{

bool shouldCover(const std::string& line, const std::string& file_name);

}

#endif
