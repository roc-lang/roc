#include "roc_std.h"

// Single translation unit compilation. Include all the source files here.
#include "lib/simpletest.cpp"
#include "alloc.cpp"
#include "str.cpp"
#include "list.cpp"
#include "box.cpp"
#include "result.cpp"

int main(int argc, char **argv)
{
    char const *groupFilter = nullptr;
    char const *nameFilter = nullptr;
    TestFixture::OutputMode output = TestFixture::Verbose;

    for (int i = 1; i < argc; ++i)
    {
        if (strcmp(argv[i], "--quiet") == 0 || strcmp(argv[i], "-q") == 0)
        {
            output = TestFixture::Normal;
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
        } else {
            fprintf(stderr, "Unknown argument: %s\n", argv[i]);
            fprintf(stderr, "Usage: %s [--quiet] [--group <group>] [--name <name>]\n", argv[0]);
            return EXIT_FAILURE;
        }
    }

    bool pass = TestFixture::ExecuteAllTests(groupFilter, nameFilter, output);
    return pass ? EXIT_SUCCESS : EXIT_FAILURE;
}
