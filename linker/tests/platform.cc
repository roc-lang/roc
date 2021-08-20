#include <iostream>

void init();
void app();
int cleanup();

void func_section_1() { init(); }

void func_section_2() { app(); }

int main() {
  std::cout << "Hello World from the platform\n";
  func_section_1();
  func_section_2();

  // Long term we want to support this case cause if it accidentially arises, we
  // don't want bugs.
  int (*func_pointer)();
  func_pointer = &cleanup;
  return (*func_pointer)();
}