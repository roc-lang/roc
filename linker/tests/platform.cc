#include <iostream>

const char* init();
const char* app();
const char* cleanup();

void func_section_1() { std::cout << init(); }

void func_section_2() { std::cout << app(); }

int main() {
  std::cout << "Hello World from the platform\n";
  func_section_1();
  func_section_2();

  // Long term we want to support this case cause if it accidentially arises, we
  // don't want bugs.
  const char* (*func_pointer)();
  func_pointer = &cleanup;
  std::cout << (*func_pointer)();
  return 0;
}