#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define HTTPSERVER_IMPL
#include "httpserver.h"

void* roc_alloc(size_t size, unsigned int alignment) { return malloc(size); }

void* roc_realloc(void* ptr, size_t old_size, size_t new_size,
                  unsigned int alignment) {
  return realloc(ptr, new_size);
}

void roc_dealloc(void* ptr, unsigned int alignment) { free(ptr); }

void roc_panic(void* ptr, unsigned int alignment) {
  char* msg = (char*)ptr;
  fprintf(stderr,
          "Application crashed with message\n\n    %s\n\nShutting down\n", msg);
  exit(0);
}

struct RocStr {
  const char* bytes;
  size_t len;
};

bool is_small_str(struct RocStr str) { return ((ssize_t)str.len) < 0; }

// Determine the length of the string, taking into
// account the small string optimization
size_t roc_str_len(struct RocStr str) {
  const char* bytes = (const char*)&str;
  char last_byte = bytes[sizeof(str) - 1];
  char last_byte_xored = last_byte ^ 0b10000000;
  size_t small_len = (size_t)(last_byte_xored);
  size_t big_len = str.len;

  // Avoid branch misprediction costs by always
  // determining both small_len and big_len,
  // so this compiles to a cmov instruction.
  if (is_small_str(str)) {
    return small_len;
  } else {
    return big_len;
  }
}

struct RocCallResult {
  size_t flag;
  struct RocStr content;
};

extern struct RocStr roc__mainForHost_1_exposed(struct RocStr url);

void handle_request(struct http_request_s* request) {
  struct http_string_s raw_url = http_request_target(request);
  struct RocStr roc_url = {raw_url.buf, raw_url.len};

  //struct RocCallResult call_result;
  struct RocStr str = roc__mainForHost_1_exposed(roc_url);

  //struct RocStr str = call_result.content;
  size_t str_len = roc_str_len(str);
  char* str_bytes = (is_small_str(str)) ? (char*)&str : str.bytes;

  struct http_response_s* response = http_response_init();
  http_response_status(response, 200);
  http_response_header(response, "Content-Type", "text/plain");
  http_response_body(response, str_bytes, str_len);
  http_respond(request, response);
}

int main() {
  struct http_server_s* server = http_server_init(8080, handle_request);
  http_server_listen(server);
}
