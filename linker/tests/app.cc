#include <iostream>

void init() { std::cout << "Application initializing...\n"; }
void app() { std::cout << "Hello World from the application\n"; }
int cleanup() {
  std::cout << "Cleaning up application...\n";
  return 0;
}
