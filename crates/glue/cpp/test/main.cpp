#include "roc_std.h"

// Single translation unit compilation. Include all the source files here.
#include "lib/simpletest.cpp"
#include "alloc.cpp"
#include "str.cpp"
#include "list.cpp"
#include "box.cpp"
#include "dict.cpp"
#include "result.cpp"

int main(int argc, char **argv)
{
    char const *groupFilter = nullptr;
    char const *nameFilter = nullptr;
    TestFixture::OutputMode output = TestFixture::Normal;

    for (int i = 1; i < argc; ++i)
    {
        if (strcmp(argv[i], "--verbose") == 0 || strcmp(argv[i], "-v") == 0)
        {
            output = TestFixture::Verbose;
        }
        else if (strcmp(argv[i], "--group") == 0 && i + 1 < argc)
        {
            groupFilter = argv[i + 1];
            ++i;
        }
        else if (strcmp(argv[i], "--name") == 0 && i + 1 < argc)
        {
            nameFilter = argv[i + 1];
            ++i;
        }
    }

    bool pass = TestFixture::ExecuteAllTests(groupFilter, nameFilter, output);
    return pass ? 0 : 1;
}
