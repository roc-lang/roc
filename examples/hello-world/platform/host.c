#include <stdio.h>

// TODO: use an actual RocStr here instead of char* - which is not the correct
// type here, and which only works out to be equivalent in this particular
// example by pure coincidence. This should be easy to segfault by returning
// (for example) small or empty strings from Hello.roc. We should fix this!
extern char* main_1();

int main() {
  printf("Roc says: %s\n", main_1());

  return 0;
}
