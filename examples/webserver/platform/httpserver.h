/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * MIT License
 *
 * Copyright (c) 2019 Jeremy Williams
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * httpserver.h (0.7.0)
 *
 * Description:
 *
 *   A single header C library for building non-blocking event driven HTTP
 *   servers
 *
 * Usage:
 *
 *   Do this:
 *      #define HTTPSERVER_IMPL
 *   before you include this file in *one* C or C++ file to create the
 *   implementation.
 *
 *   // i.e. it should look like this:
 *   #include ...
 *   #include ...
 *   #include ...
 *   #define HTTPSERVER_IMPL
 *   #include "httpserver.h"
 *
 *   There are some #defines that can be configured. This must be done in the
 *   same file that you define HTTPSERVER_IMPL These defines have default values
 *   and will need to be #undef'd and redefined to configure them.
 *
 *     HTTP_REQUEST_BUF_SIZE - default 1024 - The initial size in bytes of the
 *       read buffer for the request. This buffer grows automatically if it's
 *       capacity is reached but it certain environments it may be optimal to
 *       change this value.
 *
 *     HTTP_RESPONSE_BUF_SIZE - default 1024 - Same as above except for the
 *       response buffer.
 *
 *     HTTP_REQUEST_TIMEOUT - default 20 - The amount of seconds the request
 * will wait for activity on the socket before closing. This only applies mid
 *       request. For the amount of time to hold onto keep-alive connections see
 *       below.
 *
 *     HTTP_KEEP_ALIVE_TIMEOUT - default 120 - The amount of seconds to keep a
 *       connection alive a keep-alive request has completed.
 *
 *     HTTP_MAX_TOTAL_EST_MEM_USAGE - default 4294967296 (4GB) - This is the
 *       amount of read/write buffer space that is allowed to be allocated
 * across all requests before new requests will get 503 responses.
 *
 *     HTTP_MAX_TOKEN_LENGTH - default 8192 (8KB) - This is the max size of any
 *       non body http tokens. i.e: header names, header values, url length,
 * etc.
 *
 *     HTTP_MAX_REQUEST_BUF_SIZE - default 8388608 (8MB) - This is the maximum
 *       amount of bytes that the request buffer will grow to. If the body of
 * the request + headers cannot fit in this size the request body will be
 *       streamed in.
 *
 *   For more details see the documentation of the interface and the example
 *   below.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef HTTPSERVER_H
#define HTTPSERVER_H

#ifdef __cplusplus
extern "C" {
#endif

// String type used to read the request details. The char pointer is NOT null
// terminated.
struct http_string_s {
  char const* buf;
  int len;
};

struct http_server_s;
struct http_request_s;
struct http_response_s;

// Returns the event loop id that the server is running on. This will be an
// epoll fd when running on Linux or a kqueue on BSD. This can be used to
// listen for activity on sockets, etc. The only caveat is that the user data
// must be set to a struct where the first member is the function pointer to
// a callback that will handle the event. i.e:
//
// For kevent:
//
//   struct foo {
//     void (*handler)(struct kevent*);
//     ...
//   }
//
//   // Set ev.udata to a foo pointer when registering the event.
//
// For epoll:
//
//   struct foo {
//     void (*handler)(struct epoll_event*);
//     ...
//   }
//
//   // Set ev.data.ptr to a foo pointer when registering the event.
int http_server_loop(struct http_server_s* server);

// Allocates and initializes the http server. Takes a port and a function
// pointer that is called to process requests.
struct http_server_s* http_server_init(int port,
                                       void (*handler)(struct http_request_s*));

// Stores a pointer for future retrieval. This is not used by the library in
// any way and is strictly for you, the application programmer to make use
// of.
void http_server_set_userdata(struct http_server_s* server, void* data);

// Starts the event loop and the server listening. During normal operation this
// function will not return. Return value is the error code if the server fails
// to start. By default it will listen on all interface. For the second variant
// provide the IP address of the interface to listen on, or NULL for any.
int http_server_listen(struct http_server_s* server);
int http_server_listen_addr(struct http_server_s* server, const char* ipaddr);

// Use this listen call in place of the one above when you want to integrate
// an http server into an existing application that has a loop already and you
// want to use the polling functionality instead. This works well for
// applications like games that have a constant update loop. By default it will
// listen on all interface. For the second variant provide the IP address of
// the interface to listen on, or NULL for any.
int http_server_listen_poll(struct http_server_s* server);
int http_server_listen_addr_poll(struct http_server_s* server,
                                 const char* ipaddr);

// Call this function in your update loop. It will trigger the request handler
// once if there is a request ready. Returns 1 if a request was handled and 0
// if no requests were handled. It should be called in a loop until it returns
// 0.
int http_server_poll(struct http_server_s* server);

// Returns 1 if the flag is set and false otherwise. The flags that can be
// queried are listed below
int http_request_has_flag(struct http_request_s* request, int flag);

// This flag will be set when the request body is chunked or the body is too
// large to fit in memory are once. This means that the http_request_read_chunk
// function must be used to read the body piece by piece.
#define HTTP_FLG_STREAMED 0x1

// Returns the request method as it was read from the HTTP request line.
struct http_string_s http_request_method(struct http_request_s* request);

// Returns the full request target (url) as it was read from the HTTP request
// line.
struct http_string_s http_request_target(struct http_request_s* request);

// Returns the request body. If no request body was sent buf and len of the
// string will be set to 0.
struct http_string_s http_request_body(struct http_request_s* request);

// Returns the request header value for the given header key. The key is case
// insensitive.
struct http_string_s http_request_header(struct http_request_s* request,
                                         char const* key);

// Procedure used to iterate over all the request headers. iter should be
// initialized to zero before calling. Each call will set key and val to the
// key and value of the next header. Returns 0 when there are no more headers.
int http_request_iterate_headers(struct http_request_s* request,
                                 struct http_string_s* key,
                                 struct http_string_s* val, int* iter);

// Retrieve the opaque data pointer that was set with http_request_set_userdata.
void* http_request_userdata(struct http_request_s* request);

// Retrieve the opaque data pointer that was set with http_server_set_userdata.
void* http_request_server_userdata(struct http_request_s* request);

// Stores a pointer for future retrieval. This is not used by the library in
// any way and is strictly for you, the application programmer to make use
// of.
void http_request_set_userdata(struct http_request_s* request, void* data);

#define HTTP_KEEP_ALIVE 1
#define HTTP_CLOSE 0

// By default the server will inspect the Connection header and the HTTP
// version to determine whether the connection should be kept alive or not.
// Use this function to override that behaviour to force the connection to
// keep-alive or close by passing in the HTTP_KEEP_ALIVE or HTTP_CLOSE
// directives respectively. This may provide a minor performance improvement
// in cases where you control client and server and want to always close or
// keep the connection alive.
void http_request_connection(struct http_request_s* request, int directive);

// When reading in the HTTP request the server allocates a buffer to store
// the request details such as the headers, method, body, etc. By default this
// memory will be freed when http_respond is called. This function lets you
// free that memory before the http_respond call. This can be useful if you
// have requests that take a long time to complete and you don't require the
// request data. Accessing any http_string_s's will be invalid after this call.
void http_request_free_buffer(struct http_request_s* request);

// Allocates an http response. This memory will be freed when http_respond is
// called.
struct http_response_s* http_response_init();

// Set the response status. Accepts values between 100 and 599 inclusive. Any
// other value will map to 500.
void http_response_status(struct http_response_s* response, int status);

// Set a response header. Takes two null terminated strings.
void http_response_header(struct http_response_s* response, char const* key,
                          char const* value);

// Set the response body. The caller is responsible for freeing any memory that
// may have been allocated for the body. It is safe to free this memory AFTER
// http_respond has been called.
void http_response_body(struct http_response_s* response, char const* body,
                        int length);

// Starts writing the response to the client. Any memory allocated for the
// response body or response headers is safe to free after this call.
void http_respond(struct http_request_s* request,
                  struct http_response_s* response);

// Writes a chunk to the client. The notify_done callback will be called when
// the write is complete. This call consumes the response so a new response
// will need to be initialized for each chunk. The response status of the
// request will be the response status that is set when http_respond_chunk is
// called the first time. Any headers set for the first call will be sent as
// the response headers. Headers set for subsequent calls will be ignored.
void http_respond_chunk(struct http_request_s* request,
                        struct http_response_s* response,
                        void (*notify_done)(struct http_request_s*));

// Ends the chunked response. Any headers set before this call will be included
// as what the HTTP spec refers to as 'trailers' which are essentially more
// response headers.
void http_respond_chunk_end(struct http_request_s* request,
                            struct http_response_s* response);

// If a request has Transfer-Encoding: chunked or the body is too big to fit in
// memory all at once you cannot read the body in the typical way. Instead you
// need to call this function to read one chunk at a time. To check if the
// request requires this type of reading you can call the http_request_has_flag
// function to check if the HTTP_FLG_STREAMED flag is set. To read a streamed
// body you pass a callback that will be called when the chunk is ready. When
// the callback is called you can use `http_request_chunk` to get the current
// chunk. When done with that chunk call this function again to request the
// next chunk. If the chunk has size 0 then the request body has been completely
// read and you can now respond.
void http_request_read_chunk(struct http_request_s* request,
                             void (*chunk_cb)(struct http_request_s*));

// Returns the current chunk of the request body. This chunk is only valid until
// the next call to `http_request_read_chunk`.
struct http_string_s http_request_chunk(struct http_request_s* request);

#define http_request_read_body http_request_read_chunk

#ifdef __cplusplus
}
#endif

// Minimal example usage.
#ifdef HTTPSERVER_EXAMPLE

#define RESPONSE "Hello, World!"

void handle_request(struct http_request_s* request) {
  struct http_response_s* response = http_response_init();
  http_response_status(response, 200);
  http_response_header(response, "Content-Type", "text/plain");
  http_response_body(response, RESPONSE, sizeof(RESPONSE) - 1);
  http_respond(request, response);
}

int main() {
  struct http_server_s* server = http_server_init(8080, handle_request);
  http_server_listen(server);
}

#endif

#endif

#ifdef HTTPSERVER_IMPL
#ifndef HTTPSERVER_IMPL_ONCE
#define HTTPSERVER_IMPL_ONCE

#ifdef __linux__
#define EPOLL
#define _POSIX_C_SOURCE 199309L
#else
#define KQUEUE
#endif

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

#ifdef KQUEUE
#include <sys/event.h>
#else
#include <sys/epoll.h>
#include <sys/timerfd.h>
#endif

// *** macro definitions

// Application configurable
#define HTTP_REQUEST_BUF_SIZE 1024
#define HTTP_RESPONSE_BUF_SIZE 1024
#define HTTP_REQUEST_TIMEOUT 20
#define HTTP_KEEP_ALIVE_TIMEOUT 120
#define HTTP_MAX_TOKEN_LENGTH 8192               // 8kb
#define HTTP_MAX_TOTAL_EST_MEM_USAGE 4294967296  // 4gb
#define HTTP_MAX_REQUEST_BUF_SIZE 8388608        // 8mb

#define HTTP_MAX_HEADER_COUNT 127

#define HTTP_FLAG_SET(var, flag) var |= flag
#define HTTP_FLAG_CLEAR(var, flag) var &= ~flag
#define HTTP_FLAG_CHECK(var, flag) (var & flag)

// stream flags
#define HS_SF_CONSUMED 0x1

// parser flags
#define HS_PF_IN_CONTENT_LEN 0x1
#define HS_PF_IN_TRANSFER_ENC 0x2
#define HS_PF_CHUNKED 0x4
#define HS_PF_CKEND 0x8
#define HS_PF_REQ_END 0x10

// http session states
#define HTTP_SESSION_INIT 0
#define HTTP_SESSION_READ 1
#define HTTP_SESSION_WRITE 2
#define HTTP_SESSION_NOP 3

// http session flags
#define HTTP_END_SESSION 0x2
#define HTTP_AUTOMATIC 0x8
#define HTTP_CHUNKED_RESPONSE 0x20

// http version indicators
#define HTTP_1_0 0
#define HTTP_1_1 1

// *** declarations ***

// structs

typedef struct {
  int index;
  int len;
  int type;
} http_token_t;

typedef struct {
  http_token_t* buf;
  int capacity;
  int size;
} http_token_dyn_t;

#ifdef EPOLL
typedef void (*epoll_cb_t)(struct epoll_event*);
#endif

typedef struct http_ev_cb_s {
#ifdef KQUEUE
  void (*handler)(struct kevent* ev);
#else
  epoll_cb_t handler;
#endif
} ev_cb_t;

typedef struct {
  char* buf;
  int64_t total_bytes;
  int32_t capacity;
  int32_t length;
  int32_t index;
  int32_t anchor;
  http_token_t token;
  uint8_t flags;
} hs_stream_t;

typedef struct {
  int64_t content_length;
  int64_t body_consumed;
  int16_t match_index;
  int16_t header_count;
  int8_t state;
  int8_t meta;
} http_parser_t;

typedef struct http_request_s {
#ifdef KQUEUE
  void (*handler)(struct kevent* ev);
#else
  epoll_cb_t handler;
  epoll_cb_t timer_handler;
  int timerfd;
#endif
  void (*chunk_cb)(struct http_request_s*);
  void* data;
  hs_stream_t stream;
  http_parser_t parser;
  int state;
  int socket;
  int timeout;
  struct http_server_s* server;
  http_token_dyn_t tokens;
  char flags;
} http_request_t;

typedef struct http_server_s {
#ifdef KQUEUE
  void (*handler)(struct kevent* ev);
#else
  epoll_cb_t handler;
  epoll_cb_t timer_handler;
#endif
  int64_t memused;
  int socket;
  int port;
  int loop;
  int timerfd;
  socklen_t len;
  void (*request_handler)(http_request_t*);
  struct sockaddr_in addr;
  void* data;
  char date[32];
} http_server_t;

typedef struct http_header_s {
  char const* key;
  char const* value;
  struct http_header_s* next;
} http_header_t;

typedef struct http_response_s {
  http_header_t* headers;
  char const* body;
  int content_length;
  int status;
} http_response_t;

typedef struct http_string_s http_string_t;

// enums

enum hs_token {
  HS_TOK_NONE,
  HS_TOK_METHOD,
  HS_TOK_TARGET,
  HS_TOK_VERSION,
  HS_TOK_HEADER_KEY,
  HS_TOK_HEADER_VAL,
  HS_TOK_CHUNK_BODY,
  HS_TOK_BODY,
  HS_TOK_BODY_STREAM,
  HS_TOK_REQ_END,
  HS_TOK_EOF,
  HS_TOK_ERROR
};

enum hs_state {
  ST,
  MT,
  MS,
  TR,
  TS,
  VN,
  RR,
  RN,
  HK,
  HS,
  HV,
  HR,
  HE,
  ER,
  HN,
  BD,
  CS,
  CB,
  CE,
  CR,
  CN,
  CD,
  C1,
  C2,
  BR,
  HS_STATE_LEN
};

enum hs_char_type {
  HS_SPC,
  HS_NL,
  HS_CR,
  HS_COLN,
  HS_TAB,
  HS_SCOLN,
  HS_DIGIT,
  HS_HEX,
  HS_ALPHA,
  HS_TCHAR,
  HS_VCHAR,
  HS_ETC,
  HS_CHAR_TYPE_LEN
};

enum hs_meta_state {
  M_WFK,
  M_ANY,
  M_MTE,
  M_MCL,
  M_CLV,
  M_MCK,
  M_SML,
  M_CHK,
  M_BIG,
  M_ZER,
  M_CSZ,
  M_CBD,
  M_LST,
  M_STR,
  M_SEN,
  M_BDY,
  M_END,
  M_ERR
};

enum hs_meta_type {
  HS_META_NOT_CONTENT_LEN,
  HS_META_NOT_TRANSFER_ENC,
  HS_META_END_KEY,
  HS_META_END_VALUE,
  HS_META_END_HEADERS,
  HS_META_LARGE_BODY,
  HS_META_TYPE_LEN
};

#define HS_META_NOT_CHUNKED 0
#define HS_META_NON_ZERO 0
#define HS_META_END_CHK_SIZE 1
#define HS_META_END_CHUNK 2
#define HS_META_NEXT 0

// prototypes

void hs_add_server_sock_events(struct http_server_s* serv);
void hs_server_init(struct http_server_s* serv);
void hs_delete_events(struct http_request_s* request);
void hs_add_events(struct http_request_s* request);
void hs_add_write_event(struct http_request_s* request);
void hs_process_tokens(http_request_t* request);

#ifdef KQUEUE

void hs_server_listen_cb(struct kevent* ev);
void hs_session_io_cb(struct kevent* ev);

#else

void hs_server_listen_cb(struct epoll_event* ev);
void hs_session_io_cb(struct epoll_event* ev);
void hs_server_timer_cb(struct epoll_event* ev);
void hs_request_timer_cb(struct epoll_event* ev);

#endif

// constants

char const* hs_status_text[] = {
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "",

    // 100s
    "Continue", "Switching Protocols", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "",

    // 200s
    "OK", "Created", "Accepted", "Non-Authoritative Information", "No Content",
    "Reset Content", "Partial Content", "", "", "",

    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "",

    // 300s
    "Multiple Choices", "Moved Permanently", "Found", "See Other",
    "Not Modified", "Use Proxy", "", "Temporary Redirect", "", "",

    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "",

    // 400s
    "Bad Request", "Unauthorized", "Payment Required", "Forbidden", "Not Found",
    "Method Not Allowed", "Not Acceptable", "Proxy Authentication Required",
    "Request Timeout", "Conflict",

    "Gone", "Length Required", "", "Payload Too Large", "", "", "", "", "", "",

    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "",

    // 500s
    "Internal Server Error", "Not Implemented", "Bad Gateway",
    "Service Unavailable", "Gateway Timeout", "", "", "", "",
    ""

    "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "", "", "", "", "", "", "", "", ""};

static int const hs_transitions[] = {
    //                                            A-Z G-Z
    //                spc \n  \r  :   \t  ;   0-9 a-f g-z tch vch etc
    /* ST start */ BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    MT,
    MT,
    MT,
    BR,
    BR,
    /* MT method */ MS,
    BR,
    BR,
    BR,
    BR,
    BR,
    MT,
    MT,
    MT,
    MT,
    BR,
    BR,
    /* MS methodsp */ BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    TR,
    TR,
    TR,
    TR,
    TR,
    BR,
    /* TR target */ TS,
    BR,
    BR,
    TR,
    BR,
    TR,
    TR,
    TR,
    TR,
    TR,
    TR,
    BR,
    /* TS targetsp */ BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    VN,
    VN,
    VN,
    VN,
    VN,
    BR,
    /* VN version */ BR,
    BR,
    RR,
    BR,
    BR,
    BR,
    VN,
    VN,
    VN,
    VN,
    VN,
    BR,
    /* RR rl \r */ BR,
    RN,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    /* RN rl \n */ BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    HK,
    HK,
    HK,
    HK,
    BR,
    BR,
    /* HK headkey */ BR,
    BR,
    BR,
    HS,
    BR,
    BR,
    HK,
    HK,
    HK,
    HK,
    BR,
    BR,
    /* HS headspc */ HS,
    HS,
    HS,
    HV,
    HS,
    HV,
    HV,
    HV,
    HV,
    HV,
    HV,
    BR,
    /* HV headval */ HV,
    BR,
    HR,
    HV,
    HV,
    HV,
    HV,
    HV,
    HV,
    HV,
    HV,
    BR,
    /* HR head\r */ BR,
    HE,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    /* HE head\n */ BR,
    BR,
    ER,
    BR,
    BR,
    BR,
    HK,
    HK,
    HK,
    HK,
    BR,
    BR,
    /* ER hend\r */ BR,
    HN,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    /* HN hend\n */ BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    /* BD body */ BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    BD,
    /* CS chksz */ BR,
    BR,
    CR,
    BR,
    BR,
    CE,
    CS,
    CS,
    BR,
    BR,
    BR,
    BR,
    /* CB chkbd */ CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    /* CE chkext */ BR,
    BR,
    CR,
    CE,
    CE,
    CE,
    CE,
    CE,
    CE,
    CE,
    CE,
    BR,
    /* CR chksz\r */ BR,
    CN,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    /* CN chksz\n */ CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    CB,
    /* CD chkend */ BR,
    BR,
    C1,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    /* C1 chkend\r */ BR,
    C2,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    /* C2 chkend\n */ BR,
    BR,
    BR,
    BR,
    BR,
    BR,
    CS,
    CS,
    BR,
    BR,
    BR,
    BR};

static int const hs_meta_transitions[] = {
    //                 no chk
    //                 not cl not te endkey endval end h  toobig
    /* WFK wait */ M_WFK, M_WFK, M_WFK, M_ANY, M_END, M_ERR,
    /* ANY matchkey */ M_MTE, M_MCL, M_WFK, M_ERR, M_END, M_ERR,
    /* MTE matchte */ M_MTE, M_WFK, M_MCK, M_ERR, M_ERR, M_ERR,
    /* MCL matchcl */ M_WFK, M_MCL, M_CLV, M_ERR, M_ERR, M_ERR,
    /* CLV clvalue */ M_ERR, M_ERR, M_ERR, M_SML, M_ERR, M_ERR,
    /* MCK matchchk */ M_WFK, M_ERR, M_ERR, M_CHK, M_ERR, M_ERR,
    /* SML smallbdy */ M_SML, M_SML, M_SML, M_SML, M_BDY, M_BIG,
    /* CHK chunkbdy */ M_CHK, M_CHK, M_CHK, M_CHK, M_ZER, M_ERR,
    /* BIG bigbody */ M_BIG, M_BIG, M_BIG, M_BIG, M_STR, M_ERR,

    //                         *** chunked body ***

    //                 nonzer endsz  endchk
    /* ZER zerochk */ M_CSZ, M_LST, M_ERR, M_ERR, M_ERR, M_ERR,
    /* CSZ chksize */ M_CSZ, M_CBD, M_ERR, M_ERR, M_ERR, M_ERR,
    /* CBD readchk */ M_CBD, M_CBD, M_ZER, M_ERR, M_ERR, M_ERR,
    /* LST lastchk */ M_LST, M_END, M_END, M_ERR, M_ERR, M_ERR,

    //                         *** streamed body ***

    //                 next
    /* STR readstr */ M_SEN, M_ERR, M_ERR, M_ERR, M_ERR, M_ERR,
    /* SEN strend */ M_END, M_ERR, M_ERR, M_ERR, M_ERR, M_ERR,

    //                         *** small body ***

    //                 next
    /* BDY readbody */ M_END, M_ERR, M_ERR, M_ERR, M_ERR, M_ERR,
    /* END reqend */ M_WFK, M_ERR, M_ERR, M_ERR, M_ERR, M_ERR};

static int const hs_ctype[] = {
    HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,
    HS_ETC,   HS_ETC,   HS_TAB,   HS_NL,    HS_ETC,   HS_ETC,   HS_CR,
    HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,
    HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,
    HS_ETC,   HS_ETC,   HS_ETC,   HS_ETC,   HS_SPC,   HS_TCHAR, HS_VCHAR,
    HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_VCHAR, HS_VCHAR,
    HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_VCHAR, HS_DIGIT,
    HS_DIGIT, HS_DIGIT, HS_DIGIT, HS_DIGIT, HS_DIGIT, HS_DIGIT, HS_DIGIT,
    HS_DIGIT, HS_DIGIT, HS_COLN,  HS_SCOLN, HS_VCHAR, HS_VCHAR, HS_VCHAR,
    HS_VCHAR, HS_VCHAR, HS_HEX,   HS_HEX,   HS_HEX,   HS_HEX,   HS_HEX,
    HS_HEX,   HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA,
    HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA,
    HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA,
    HS_VCHAR, HS_VCHAR, HS_VCHAR, HS_TCHAR, HS_TCHAR, HS_TCHAR, HS_HEX,
    HS_HEX,   HS_HEX,   HS_HEX,   HS_HEX,   HS_HEX,   HS_ALPHA, HS_ALPHA,
    HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA,
    HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA,
    HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_ALPHA, HS_VCHAR, HS_TCHAR, HS_VCHAR,
    HS_TCHAR, HS_ETC};

static int const hs_token_start_states[] =
    {
        // ST MT             MS TR             TS VN              RR RN HK
        0,
        HS_TOK_METHOD,
        0,
        HS_TOK_TARGET,
        0,
        HS_TOK_VERSION,
        0,
        0,
        HS_TOK_HEADER_KEY,
        // HS HV                 HR HE ER HN BD           CS CB CE CR CN
        0,
        HS_TOK_HEADER_VAL,
        0,
        0,
        0,
        0,
        HS_TOK_BODY,
        0,
        HS_TOK_CHUNK_BODY,
        0,
        0,
        0,
        // CD C1 C2
        0,
        0,
        0,
};

// *** input stream ***

int hs_stream_read_socket(hs_stream_t* stream, int socket, int64_t* memused) {
  if (stream->index < stream->length) return 1;
  if (!stream->buf) {
    *memused += HTTP_REQUEST_BUF_SIZE;
    stream->buf = (char*)calloc(1, HTTP_REQUEST_BUF_SIZE);
    assert(stream->buf != NULL);
    stream->capacity = HTTP_REQUEST_BUF_SIZE;
  }
  int bytes;
  do {
    bytes = read(socket, stream->buf + stream->length,
                 stream->capacity - stream->length);
    if (bytes > 0) {
      stream->length += bytes;
      stream->total_bytes += bytes;
    }
    if (stream->length == stream->capacity &&
        stream->capacity != HTTP_MAX_REQUEST_BUF_SIZE) {
      *memused -= stream->capacity;
      stream->capacity *= 2;
      if (stream->capacity > HTTP_MAX_REQUEST_BUF_SIZE) {
        stream->capacity = HTTP_MAX_REQUEST_BUF_SIZE;
      }
      *memused += stream->capacity;
      stream->buf = (char*)realloc(stream->buf, stream->capacity);
      assert(stream->buf != NULL);
    }
  } while (bytes > 0 && stream->capacity < HTTP_MAX_REQUEST_BUF_SIZE);
  return bytes == 0 ? 0 : 1;
}

int hs_stream_next(hs_stream_t* stream, char* c) {
  HTTP_FLAG_CLEAR(stream->flags, HS_SF_CONSUMED);
  if (stream->index >= stream->length) return 0;
  *c = stream->buf[stream->index];
  return 1;
}

void hs_stream_consume(hs_stream_t* stream) {
  if (HTTP_FLAG_CHECK(stream->flags, HS_SF_CONSUMED)) return;
  HTTP_FLAG_SET(stream->flags, HS_SF_CONSUMED);
  stream->index++;
  int new_len = stream->token.len + 1;
  stream->token.len = stream->token.type == 0 ? 0 : new_len;
}

void hs_stream_begin_token(hs_stream_t* stream, int token_type) {
  stream->token.index = stream->index;
  stream->token.type = token_type;
}

int hs_stream_jump(hs_stream_t* stream, int offset) {
  HTTP_FLAG_SET(stream->flags, HS_SF_CONSUMED);
  if (stream->index + offset > stream->length) return 0;
  stream->index += offset;
  int new_len = stream->token.len + offset;
  stream->token.len = stream->token.type == 0 ? 0 : new_len;
  return 1;
}

int hs_stream_jumpall(hs_stream_t* stream) {
  int offset = stream->length - stream->index;
  stream->index += offset;
  int new_len = stream->token.len + offset;
  HTTP_FLAG_SET(stream->flags, HS_SF_CONSUMED);
  stream->token.len = stream->token.type == 0 ? 0 : new_len;
  return offset;
}

void hs_stream_anchor(hs_stream_t* stream) { stream->anchor = stream->index; }

http_token_t hs_stream_emit(hs_stream_t* stream) {
  http_token_t token = stream->token;
  http_token_t none = {0, 0, 0};
  stream->token = none;
  return token;
}

http_token_t hs_stream_current_token(hs_stream_t* stream) {
  return stream->token;
}

int hs_stream_can_contain(hs_stream_t* stream, int64_t size) {
  return HTTP_MAX_REQUEST_BUF_SIZE - stream->index + 1 >= size;
}

void hs_stream_shift(hs_stream_t* stream) {
  if (stream->token.index == stream->anchor) return;
  if (stream->token.len > 0) {
    char* dst = stream->buf + stream->anchor;
    char const* src = stream->buf + stream->token.index;
    int bytes = stream->length - stream->token.index;
    memcpy(dst, src, bytes);
  }
  stream->token.index = stream->anchor;
  stream->index = stream->token.len + stream->anchor;
  stream->length = stream->anchor + stream->token.len;
}

// *** http parser ***

void hs_trigger_meta(http_parser_t* parser, int event) {
  int to = hs_meta_transitions[parser->meta * HS_META_TYPE_LEN + event];
  parser->meta = to;
}

#define HS_MATCH(str, meta)                               \
  in_bounds = parser->match_index < (int)sizeof(str) - 1; \
  m = in_bounds ? str[parser->match_index] : m;           \
  low = c >= 'A' && c <= 'Z' ? c + 32 : c;                \
  if (low != m) hs_trigger_meta(parser, meta);

http_token_t hs_transition_action(http_parser_t* parser, hs_stream_t* stream,
                                  char c, int8_t from, int8_t to) {
  http_token_t emitted = {0, 0, 0};
  if (from == HN) {
    hs_stream_anchor(stream);
  }
  if (from != to) {
    int type = hs_token_start_states[to];
    if (type != HS_TOK_NONE) hs_stream_begin_token(stream, type);
    if (from == CS) hs_trigger_meta(parser, HS_META_END_CHK_SIZE);
    if (to == HK) {
      parser->header_count++;
      if (parser->header_count > HTTP_MAX_HEADER_COUNT) {
        emitted.type = HS_TOK_ERROR;
      }
    } else if (to == HS) {
      hs_trigger_meta(parser, HS_META_END_KEY);
      emitted = hs_stream_emit(stream);
    }
    parser->match_index = 0;
  }
  char low, m = '\0';
  int in_bounds = 0;
  int body_left = 0;
  switch (to) {
    case MS:
    case TS:
      emitted = hs_stream_emit(stream);
      break;
    case RR:
    case HR:
      hs_trigger_meta(parser, HS_META_END_VALUE);
      emitted = hs_stream_emit(stream);
      break;
    case HK:
      HS_MATCH("transfer-encoding", HS_META_NOT_TRANSFER_ENC)
      HS_MATCH("content-length", HS_META_NOT_CONTENT_LEN)
      parser->match_index++;
      break;
    case HV:
      if (parser->meta == M_MCK) {
        HS_MATCH("chunked", HS_META_NOT_CHUNKED)
        parser->match_index++;
      } else if (parser->meta == M_CLV) {
        parser->content_length *= 10;
        parser->content_length += c - '0';
      }
      break;
    case HN:
      if (parser->meta == M_SML &&
          !hs_stream_can_contain(stream, parser->content_length)) {
        hs_trigger_meta(parser, HS_META_LARGE_BODY);
      }
      if (parser->meta == M_BIG || parser->meta == M_CHK) {
        emitted.type = HS_TOK_BODY_STREAM;
      }
      // if (parser->meta == M_CHK) parser->state = CS;
      hs_trigger_meta(parser, HS_META_END_HEADERS);
      if (parser->content_length == 0 && parser->meta == M_BDY)
        parser->meta = M_END;
      if (parser->meta == M_END) {
        emitted.type = HS_TOK_BODY;
      }
      break;
    case CS:
      if (c != '0') hs_trigger_meta(parser, HS_META_NON_ZERO);
      if (c >= 'A' && c <= 'F') {
        parser->content_length *= 0x10;
        parser->content_length += c - 55;
      } else if (c >= 'a' && c <= 'f') {
        parser->content_length *= 0x10;
        parser->content_length += c - 87;
      } else if (c >= '0' && c <= '9') {
        parser->content_length *= 0x10;
        parser->content_length += c - '0';
      }
      break;
    case CB:
    case BD:
      if (parser->meta == M_STR)
        hs_stream_begin_token(stream, HS_TOK_CHUNK_BODY);
      body_left = parser->content_length - parser->body_consumed;
      if (hs_stream_jump(stream, body_left)) {
        emitted = hs_stream_emit(stream);
        hs_trigger_meta(parser, HS_META_NEXT);
        if (to == CB) parser->state = CD;
        parser->content_length = 0;
        parser->body_consumed = 0;
      } else {
        parser->body_consumed += hs_stream_jumpall(stream);
        if (parser->meta == M_STR) {
          emitted = hs_stream_emit(stream);
          hs_stream_shift(stream);
        }
      }
      break;
    case C2:
      hs_trigger_meta(parser, HS_META_END_CHUNK);
      break;
    case BR:
      emitted.type = HS_TOK_ERROR;
      break;
  }
  return emitted;
}

http_token_t hs_meta_emit(http_parser_t* parser) {
  http_token_t token = {0, 0, 0};
  switch (parser->meta) {
    case M_SEN:
      token.type = HS_TOK_CHUNK_BODY;
      hs_trigger_meta(parser, HS_META_NEXT);
      break;
    case M_END:
      token.type = HS_TOK_REQ_END;
      memset(parser, 0, sizeof(http_parser_t));
      break;
  }
  return token;
}

http_token_t http_parse(http_parser_t* parser, hs_stream_t* stream) {
  char c = 0;
  http_token_t token = hs_meta_emit(parser);
  if (token.type != HS_TOK_NONE) return token;
  while (hs_stream_next(stream, &c)) {
    int type = c < 0 ? HS_ETC : hs_ctype[(int)c];
    int to = hs_transitions[parser->state * HS_CHAR_TYPE_LEN + type];
    if (parser->meta == M_ZER && parser->state == HN && to == BD) {
      to = CS;
    }
    int from = parser->state;
    parser->state = to;
    http_token_t emitted = hs_transition_action(parser, stream, c, from, to);
    hs_stream_consume(stream);
    if (emitted.type != HS_TOK_NONE) return emitted;
  }
  if (parser->state == CB) hs_stream_shift(stream);
  token = hs_meta_emit(parser);
  http_token_t current = hs_stream_current_token(stream);
  if (current.type != HS_TOK_CHUNK_BODY && current.type != HS_TOK_BODY &&
      current.len > HTTP_MAX_TOKEN_LENGTH) {
    token.type = HS_TOK_ERROR;
  }
  return token;
}

// *** http server ***

void http_token_dyn_push(http_token_dyn_t* dyn, http_token_t a) {
  if (dyn->size == dyn->capacity) {
    dyn->capacity *= 2;
    dyn->buf =
        (http_token_t*)realloc(dyn->buf, dyn->capacity * sizeof(http_token_t));
    assert(dyn->buf != NULL);
  }
  dyn->buf[dyn->size] = a;
  dyn->size++;
}

void http_token_dyn_init(http_token_dyn_t* dyn, int capacity) {
  dyn->buf = (http_token_t*)malloc(sizeof(http_token_t) * capacity);
  assert(dyn->buf != NULL);
  dyn->size = 0;
  dyn->capacity = capacity;
}

void hs_bind_localhost(int s, struct sockaddr_in* addr, const char* ipaddr,
                       int port) {
  addr->sin_family = AF_INET;
  if (ipaddr == NULL) {
    addr->sin_addr.s_addr = INADDR_ANY;
  } else {
    addr->sin_addr.s_addr = inet_addr(ipaddr);
  }
  addr->sin_port = htons(port);
  int rc = bind(s, (struct sockaddr*)addr, sizeof(struct sockaddr_in));
  if (rc < 0) {
    exit(1);
  }
}

int hs_write_client_socket(http_request_t* session) {
  int bytes =
      write(session->socket, session->stream.buf + session->stream.total_bytes,
            session->stream.length - session->stream.total_bytes);
  if (bytes > 0) session->stream.total_bytes += bytes;
  return errno == EPIPE ? 0 : 1;
}

void hs_free_buffer(http_request_t* session) {
  if (session->stream.buf) {
    free(session->stream.buf);
    session->server->memused -= session->stream.capacity;
    session->stream.buf = NULL;
  }
}

void hs_init_session(http_request_t* session) {
  session->flags = HTTP_AUTOMATIC;
  session->parser = (http_parser_t){};
  session->stream = (hs_stream_t){};
  if (session->tokens.buf) {
    free(session->tokens.buf);
    session->tokens.buf = NULL;
  }
  http_token_dyn_init(&session->tokens, 32);
}

void hs_end_session(http_request_t* session) {
  hs_delete_events(session);
  close(session->socket);
  hs_free_buffer(session);
  free(session->tokens.buf);
  session->tokens.buf = NULL;
  free(session);
}

void hs_reset_timeout(http_request_t* request, int time) {
  request->timeout = time;
}

void hs_read_and_process_request(http_request_t* request);

void hs_write_response(http_request_t* request) {
  if (!hs_write_client_socket(request)) {
    HTTP_FLAG_SET(request->flags, HTTP_END_SESSION);
    return;
  }
  if (request->stream.total_bytes != request->stream.length) {
    // All bytes of the body were not written and we need to wait until the
    // socket is writable again to complete the write
    hs_add_write_event(request);
    request->state = HTTP_SESSION_WRITE;
    hs_reset_timeout(request, HTTP_REQUEST_TIMEOUT);
  } else if (HTTP_FLAG_CHECK(request->flags, HTTP_CHUNKED_RESPONSE)) {
    // All bytes of the chunk were written and we need to get the next chunk
    // from the application.
    request->state = HTTP_SESSION_WRITE;
    hs_reset_timeout(request, HTTP_REQUEST_TIMEOUT);
    hs_free_buffer(request);
    request->chunk_cb(request);
  } else {
    if (HTTP_FLAG_CHECK(request->flags, HTTP_KEEP_ALIVE)) {
      request->state = HTTP_SESSION_INIT;
      hs_free_buffer(request);
      hs_reset_timeout(request, HTTP_KEEP_ALIVE_TIMEOUT);
    } else {
      HTTP_FLAG_SET(request->flags, HTTP_END_SESSION);
    }
  }
}

void hs_error_response(http_request_t* request, int code, char const* message) {
  struct http_response_s* response = http_response_init();
  http_response_status(response, code);
  http_response_header(response, "Content-Type", "text/plain");
  http_response_body(response, message, strlen(message));
  http_respond(request, response);
  hs_write_response(request);
}

void hs_read_and_process_request(http_request_t* request) {
  request->state = HTTP_SESSION_READ;
  http_token_t token = {0, 0, 0};
  hs_reset_timeout(request, HTTP_REQUEST_TIMEOUT);
  int rc = hs_stream_read_socket(&request->stream, request->socket,
                                 &request->server->memused);
  if (rc == 0) {
    HTTP_FLAG_SET(request->flags, HTTP_END_SESSION);
    return;
  }
  do {
    token = http_parse(&request->parser, &request->stream);
    if (token.type != HS_TOK_NONE) http_token_dyn_push(&request->tokens, token);
    switch (token.type) {
      case HS_TOK_ERROR:
        hs_error_response(request, 400, "Bad Request");
        break;
      case HS_TOK_BODY:
      case HS_TOK_BODY_STREAM:
        if (token.type == HS_TOK_BODY_STREAM) {
          HTTP_FLAG_SET(request->flags, HTTP_FLG_STREAMED);
        }
        request->state = HTTP_SESSION_NOP;
        request->server->request_handler(request);
        break;
      case HS_TOK_CHUNK_BODY:
        request->state = HTTP_SESSION_NOP;
        request->chunk_cb(request);
        break;
    }
  } while (token.type != HS_TOK_NONE && request->state == HTTP_SESSION_READ);
}

// Application requesting next chunk of request body.
void http_request_read_chunk(struct http_request_s* request,
                             void (*chunk_cb)(struct http_request_s*)) {
  request->chunk_cb = chunk_cb;
  hs_read_and_process_request(request);
}

// This is the heart of the request logic. This is the state machine that
// controls what happens when an IO event is received.
void http_session(http_request_t* request) {
  switch (request->state) {
    case HTTP_SESSION_INIT:
      hs_init_session(request);
      request->state = HTTP_SESSION_READ;
      if (request->server->memused > HTTP_MAX_TOTAL_EST_MEM_USAGE) {
        return hs_error_response(request, 503, "Service Unavailable");
      }
      // fallthrough
    case HTTP_SESSION_READ:
      hs_read_and_process_request(request);
      break;
    case HTTP_SESSION_WRITE:
      hs_write_response(request);
      break;
  }
  if (HTTP_FLAG_CHECK(request->flags, HTTP_END_SESSION)) {
    hs_end_session(request);
  }
}

void hs_accept_connections(http_server_t* server) {
  int sock = 0;
  do {
    sock =
        accept(server->socket, (struct sockaddr*)&server->addr, &server->len);
    if (sock > 0) {
      http_request_t* session =
          (http_request_t*)calloc(1, sizeof(http_request_t));
      assert(session != NULL);
      session->socket = sock;
      session->server = server;
      session->timeout = HTTP_REQUEST_TIMEOUT;
      session->handler = hs_session_io_cb;
      int flags = fcntl(sock, F_GETFL, 0);
      fcntl(sock, F_SETFL, flags | O_NONBLOCK);
      hs_add_events(session);
      http_session(session);
    }
  } while (sock > 0);
}

void hs_generate_date_time(char* datetime) {
  time_t rawtime;
  struct tm* timeinfo;
  time(&rawtime);
  timeinfo = gmtime(&rawtime);
  strftime(datetime, 32, "%a, %d %b %Y %T GMT", timeinfo);
}

http_server_t* http_server_init(int port, void (*handler)(http_request_t*)) {
  http_server_t* serv = (http_server_t*)malloc(sizeof(http_server_t));
  assert(serv != NULL);
  serv->port = port;
  serv->memused = 0;
  serv->handler = hs_server_listen_cb;
  hs_server_init(serv);
  hs_generate_date_time(serv->date);
  serv->request_handler = handler;
  return serv;
}

void http_server_set_userdata(struct http_server_s* serv, void* data) {
  serv->data = data;
}

void http_listen(http_server_t* serv, const char* ipaddr) {
  // Ignore SIGPIPE. We handle these errors at the call site.
  signal(SIGPIPE, SIG_IGN);
  serv->socket = socket(AF_INET, SOCK_STREAM, 0);
  int flag = 1;
  setsockopt(serv->socket, SOL_SOCKET, SO_REUSEPORT, &flag, sizeof(flag));
  hs_bind_localhost(serv->socket, &serv->addr, ipaddr, serv->port);
  serv->len = sizeof(serv->addr);
  int flags = fcntl(serv->socket, F_GETFL, 0);
  fcntl(serv->socket, F_SETFL, flags | O_NONBLOCK);
  listen(serv->socket, 128);
  hs_add_server_sock_events(serv);
}

int http_server_listen_addr_poll(http_server_t* serv, const char* ipaddr) {
  http_listen(serv, ipaddr);
  return 0;
}

int http_server_listen_poll(http_server_t* serv) {
  return http_server_listen_addr_poll(serv, NULL);
}

int http_server_loop(http_server_t* server) { return server->loop; }

// *** http request ***

http_string_t http_get_token_string(http_request_t* request, int token_type) {
  http_string_t str = {0, 0};
  if (request->tokens.buf == NULL) return str;
  for (int i = 0; i < request->tokens.size; i++) {
    http_token_t token = request->tokens.buf[i];
    if (token.type == token_type) {
      str.buf = &request->stream.buf[token.index];
      str.len = token.len;
      return str;
    }
  }
  return str;
}

int http_request_has_flag(http_request_t* request, int flag) {
  return HTTP_FLAG_CHECK(request->flags, flag);
}

int hs_case_insensitive_cmp(char const* a, char const* b, int len) {
  for (int i = 0; i < len; i++) {
    char c1 = a[i] >= 'A' && a[i] <= 'Z' ? a[i] + 32 : a[i];
    char c2 = b[i] >= 'A' && b[i] <= 'Z' ? b[i] + 32 : b[i];
    if (c1 != c2) return 0;
  }
  return 1;
}

http_string_t http_request_method(http_request_t* request) {
  return http_get_token_string(request, HS_TOK_METHOD);
}

http_string_t http_request_target(http_request_t* request) {
  return http_get_token_string(request, HS_TOK_TARGET);
}

http_string_t http_request_body(http_request_t* request) {
  return http_get_token_string(request, HS_TOK_BODY);
}

int hs_assign_iteration_headers(http_request_t* request, http_string_t* key,
                                http_string_t* val, int* iter) {
  http_token_t token = request->tokens.buf[*iter];
  if (request->tokens.buf[*iter].type == HS_TOK_BODY) return 0;
  *key = (http_string_t){.buf = &request->stream.buf[token.index],
                         .len = token.len};
  (*iter)++;
  token = request->tokens.buf[*iter];
  *val = (http_string_t){.buf = &request->stream.buf[token.index],
                         .len = token.len};
  return 1;
}

int http_request_iterate_headers(http_request_t* request, http_string_t* key,
                                 http_string_t* val, int* iter) {
  if (*iter == 0) {
    for (; *iter < request->tokens.size; (*iter)++) {
      http_token_t token = request->tokens.buf[*iter];
      if (token.type == HS_TOK_HEADER_KEY) {
        return hs_assign_iteration_headers(request, key, val, iter);
      }
    }
    return 0;
  } else {
    (*iter)++;
    return hs_assign_iteration_headers(request, key, val, iter);
  }
}

http_string_t http_request_header(http_request_t* request, char const* key) {
  int len = strlen(key);
  for (int i = 0; i < request->tokens.size; i++) {
    http_token_t token = request->tokens.buf[i];
    if (token.type == HS_TOK_HEADER_KEY && token.len == len) {
      if (hs_case_insensitive_cmp(&request->stream.buf[token.index], key,
                                  len)) {
        token = request->tokens.buf[i + 1];
        return (http_string_t){.buf = &request->stream.buf[token.index],
                               .len = token.len};
      }
    }
  }
  return (http_string_t){};
}

void http_request_free_buffer(http_request_t* request) {
  hs_free_buffer(request);
}

void* http_request_userdata(http_request_t* request) { return request->data; }

void http_request_set_userdata(http_request_t* request, void* data) {
  request->data = data;
}

void* http_request_server_userdata(struct http_request_s* request) {
  return request->server->data;
}

void hs_auto_detect_keep_alive(http_request_t* request) {
  http_string_t str = http_get_token_string(request, HS_TOK_VERSION);
  if (str.buf == NULL) return;
  int version = str.buf[str.len - 1] == '1';
  str = http_request_header(request, "Connection");
  if ((str.len == 5 && hs_case_insensitive_cmp(str.buf, "close", 5)) ||
      (str.len == 0 && version == HTTP_1_0)) {
    HTTP_FLAG_CLEAR(request->flags, HTTP_KEEP_ALIVE);
  } else {
    HTTP_FLAG_SET(request->flags, HTTP_KEEP_ALIVE);
  }
}

void http_request_connection(http_request_t* request, int directive) {
  if (directive == HTTP_KEEP_ALIVE) {
    HTTP_FLAG_CLEAR(request->flags, HTTP_AUTOMATIC);
    HTTP_FLAG_SET(request->flags, HTTP_KEEP_ALIVE);
  } else if (directive == HTTP_CLOSE) {
    HTTP_FLAG_CLEAR(request->flags, HTTP_AUTOMATIC);
    HTTP_FLAG_CLEAR(request->flags, HTTP_KEEP_ALIVE);
  }
}

http_string_t http_request_chunk(struct http_request_s* request) {
  http_token_t token = request->tokens.buf[request->tokens.size - 1];
  return (http_string_t){.buf = &request->stream.buf[token.index],
                         .len = token.len};
}

// *** http response ***

http_response_t* http_response_init() {
  http_response_t* response =
      (http_response_t*)calloc(1, sizeof(http_response_t));
  assert(response != NULL);
  response->status = 200;
  return response;
}

void http_response_header(http_response_t* response, char const* key,
                          char const* value) {
  http_header_t* header = (http_header_t*)malloc(sizeof(http_header_t));
  assert(header != NULL);
  header->key = key;
  header->value = value;
  http_header_t* prev = response->headers;
  header->next = prev;
  response->headers = header;
}

void http_response_status(http_response_t* response, int status) {
  response->status = status > 599 || status < 100 ? 500 : status;
}

void http_response_body(http_response_t* response, char const* body,
                        int length) {
  response->body = body;
  response->content_length = length;
}

typedef struct {
  char* buf;
  int capacity;
  int size;
  int64_t* memused;
} grwprintf_t;

void grwprintf_init(grwprintf_t* ctx, int capacity, int64_t* memused) {
  ctx->memused = memused;
  ctx->size = 0;
  ctx->buf = (char*)malloc(capacity);
  *ctx->memused += capacity;
  assert(ctx->buf != NULL);
  ctx->capacity = capacity;
}

void grwmemcpy(grwprintf_t* ctx, char const* src, int size) {
  if (ctx->size + size > ctx->capacity) {
    *ctx->memused -= ctx->capacity;
    ctx->capacity = ctx->size + size;
    *ctx->memused += ctx->capacity;
    ctx->buf = (char*)realloc(ctx->buf, ctx->capacity);
    assert(ctx->buf != NULL);
  }
  memcpy(ctx->buf + ctx->size, src, size);
  ctx->size += size;
}

void grwprintf(grwprintf_t* ctx, char const* fmt, ...) {
  va_list args;
  va_start(args, fmt);

  int bytes =
      vsnprintf(ctx->buf + ctx->size, ctx->capacity - ctx->size, fmt, args);
  if (bytes + ctx->size > ctx->capacity) {
    *ctx->memused -= ctx->capacity;
    while (bytes + ctx->size > ctx->capacity) ctx->capacity *= 2;
    *ctx->memused += ctx->capacity;
    ctx->buf = (char*)realloc(ctx->buf, ctx->capacity);
    assert(ctx->buf != NULL);
    bytes +=
        vsnprintf(ctx->buf + ctx->size, ctx->capacity - ctx->size, fmt, args);
  }
  ctx->size += bytes;

  va_end(args);
}

void http_buffer_headers(http_request_t* request, http_response_t* response,
                         grwprintf_t* printctx) {
  http_header_t* header = response->headers;
  while (header) {
    grwprintf(printctx, "%s: %s\r\n", header->key, header->value);
    header = header->next;
  }
  if (!HTTP_FLAG_CHECK(request->flags, HTTP_CHUNKED_RESPONSE)) {
    grwprintf(printctx, "Content-Length: %d\r\n", response->content_length);
  }
  grwprintf(printctx, "\r\n");
}

void http_respond_headers(http_request_t* request, http_response_t* response,
                          grwprintf_t* printctx) {
  if (HTTP_FLAG_CHECK(request->flags, HTTP_AUTOMATIC)) {
    hs_auto_detect_keep_alive(request);
  }
  if (HTTP_FLAG_CHECK(request->flags, HTTP_KEEP_ALIVE)) {
    http_response_header(response, "Connection", "keep-alive");
  } else {
    http_response_header(response, "Connection", "close");
  }
  grwprintf(printctx, "HTTP/1.1 %d %s\r\nDate: %s\r\n", response->status,
            hs_status_text[response->status], request->server->date);
  http_buffer_headers(request, response, printctx);
}

void http_end_response(http_request_t* request, http_response_t* response,
                       grwprintf_t* printctx) {
  http_header_t* header = response->headers;
  while (header) {
    http_header_t* tmp = header;
    header = tmp->next;
    free(tmp);
  }
  hs_free_buffer(request);
  free(response);
  request->stream.buf = printctx->buf;
  request->stream.total_bytes = 0;
  request->stream.length = printctx->size;
  request->stream.capacity = printctx->capacity;
  request->state = HTTP_SESSION_WRITE;
  hs_write_response(request);
}

void http_respond(http_request_t* request, http_response_t* response) {
  grwprintf_t printctx;
  grwprintf_init(&printctx, HTTP_RESPONSE_BUF_SIZE, &request->server->memused);
  http_respond_headers(request, response, &printctx);
  if (response->body) {
    grwmemcpy(&printctx, response->body, response->content_length);
  }
  http_end_response(request, response, &printctx);
}

void http_respond_chunk(http_request_t* request, http_response_t* response,
                        void (*cb)(http_request_t*)) {
  grwprintf_t printctx;
  grwprintf_init(&printctx, HTTP_RESPONSE_BUF_SIZE, &request->server->memused);
  if (!HTTP_FLAG_CHECK(request->flags, HTTP_CHUNKED_RESPONSE)) {
    HTTP_FLAG_SET(request->flags, HTTP_CHUNKED_RESPONSE);
    http_response_header(response, "Transfer-Encoding", "chunked");
    http_respond_headers(request, response, &printctx);
  }
  request->chunk_cb = cb;
  grwprintf(&printctx, "%X\r\n", response->content_length);
  grwmemcpy(&printctx, response->body, response->content_length);
  grwprintf(&printctx, "\r\n");
  http_end_response(request, response, &printctx);
}

void http_respond_chunk_end(http_request_t* request,
                            http_response_t* response) {
  grwprintf_t printctx;
  grwprintf_init(&printctx, HTTP_RESPONSE_BUF_SIZE, &request->server->memused);
  grwprintf(&printctx, "0\r\n");
  http_buffer_headers(request, response, &printctx);
  grwprintf(&printctx, "\r\n");
  HTTP_FLAG_CLEAR(request->flags, HTTP_CHUNKED_RESPONSE);
  http_end_response(request, response, &printctx);
}

// *** kqueue platform specific ***

#ifdef KQUEUE

void hs_server_listen_cb(struct kevent* ev) {
  http_server_t* server = (http_server_t*)ev->udata;
  if (ev->filter == EVFILT_TIMER) {
    hs_generate_date_time(server->date);
  } else {
    hs_accept_connections(server);
  }
}

void hs_session_io_cb(struct kevent* ev) {
  http_request_t* request = (http_request_t*)ev->udata;
  if (ev->filter == EVFILT_TIMER) {
    request->timeout -= 1;
    if (request->timeout == 0) hs_end_session(request);
  } else {
    http_session(request);
  }
}

void hs_server_init(http_server_t* serv) {
  serv->loop = kqueue();
  struct kevent ev_set;
  EV_SET(&ev_set, 1, EVFILT_TIMER, EV_ADD | EV_ENABLE, 0, 1000, serv);
  kevent(serv->loop, &ev_set, 1, NULL, 0, NULL);
}

void hs_add_server_sock_events(http_server_t* serv) {
  struct kevent ev_set;
  EV_SET(&ev_set, serv->socket, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, serv);
  kevent(serv->loop, &ev_set, 1, NULL, 0, NULL);
}

int http_server_listen_addr(http_server_t* serv, const char* ipaddr) {
  http_listen(serv, ipaddr);

  struct kevent ev_list[1];

  while (1) {
    int nev = kevent(serv->loop, NULL, 0, ev_list, 1, NULL);
    for (int i = 0; i < nev; i++) {
      ev_cb_t* ev_cb = (ev_cb_t*)ev_list[i].udata;
      ev_cb->handler(&ev_list[i]);
    }
  }
  return 0;
}

int http_server_listen(http_server_t* serv) {
  return http_server_listen_addr(serv, NULL);
}

void hs_delete_events(http_request_t* request) {
  struct kevent ev_set;
  EV_SET(&ev_set, request->socket, EVFILT_TIMER, EV_DELETE, 0, 0, request);
  kevent(request->server->loop, &ev_set, 1, NULL, 0, NULL);
}

int http_server_poll(http_server_t* serv) {
  struct kevent ev;
  struct timespec ts = {0, 0};
  int nev = kevent(serv->loop, NULL, 0, &ev, 1, &ts);
  if (nev <= 0) return nev;
  ev_cb_t* ev_cb = (ev_cb_t*)ev.udata;
  ev_cb->handler(&ev);
  return nev;
}

void hs_add_events(http_request_t* request) {
  struct kevent ev_set[2];
  EV_SET(&ev_set[0], request->socket, EVFILT_READ, EV_ADD, 0, 0, request);
  EV_SET(&ev_set[1], request->socket, EVFILT_TIMER, EV_ADD | EV_ENABLE, 0, 1000,
         request);
  kevent(request->server->loop, ev_set, 2, NULL, 0, NULL);
}

void hs_add_write_event(http_request_t* request) {
  struct kevent ev_set[2];
  EV_SET(&ev_set[0], request->socket, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0,
         request);
  kevent(request->server->loop, ev_set, 2, NULL, 0, NULL);
}

#else

// *** epoll platform specific ***

void hs_server_listen_cb(struct epoll_event* ev) {
  hs_accept_connections((http_server_t*)ev->data.ptr);
}

void hs_session_io_cb(struct epoll_event* ev) {
  http_session((http_request_t*)ev->data.ptr);
}

void hs_server_timer_cb(struct epoll_event* ev) {
  http_server_t* server =
      (http_server_t*)((char*)ev->data.ptr - sizeof(epoll_cb_t));
  uint64_t res;
  int bytes = read(server->timerfd, &res, sizeof(res));
  (void)bytes;  // suppress warning
  hs_generate_date_time(server->date);
}

void hs_request_timer_cb(struct epoll_event* ev) {
  http_request_t* request =
      (http_request_t*)((char*)ev->data.ptr - sizeof(epoll_cb_t));
  uint64_t res;
  int bytes = read(request->timerfd, &res, sizeof(res));
  (void)bytes;  // suppress warning
  request->timeout -= 1;
  if (request->timeout == 0) hs_end_session(request);
}

void hs_add_server_sock_events(http_server_t* serv) {
  struct epoll_event ev;
  ev.events = EPOLLIN | EPOLLET;
  ev.data.ptr = serv;
  epoll_ctl(serv->loop, EPOLL_CTL_ADD, serv->socket, &ev);
}

void hs_server_init(http_server_t* serv) {
  serv->loop = epoll_create1(0);
  serv->timer_handler = hs_server_timer_cb;

  int tfd = timerfd_create(CLOCK_MONOTONIC, 0);
  struct itimerspec ts = {};
  ts.it_value.tv_sec = 1;
  ts.it_interval.tv_sec = 1;
  timerfd_settime(tfd, 0, &ts, NULL);

  struct epoll_event ev;
  ev.events = EPOLLIN | EPOLLET;
  ev.data.ptr = &serv->timer_handler;
  epoll_ctl(serv->loop, EPOLL_CTL_ADD, tfd, &ev);
  serv->timerfd = tfd;
}

int http_server_listen_addr(http_server_t* serv, const char* ipaddr) {
  http_listen(serv, ipaddr);
  struct epoll_event ev_list[1];
  while (1) {
    int nev = epoll_wait(serv->loop, ev_list, 1, -1);
    for (int i = 0; i < nev; i++) {
      ev_cb_t* ev_cb = (ev_cb_t*)ev_list[i].data.ptr;
      ev_cb->handler(&ev_list[i]);
    }
  }
  return 0;
}

int http_server_listen(http_server_t* serv) {
  return http_server_listen_addr(serv, NULL);
}

void hs_delete_events(http_request_t* request) {
  epoll_ctl(request->server->loop, EPOLL_CTL_DEL, request->socket, NULL);
  epoll_ctl(request->server->loop, EPOLL_CTL_DEL, request->timerfd, NULL);
  close(request->timerfd);
}

int http_server_poll(http_server_t* serv) {
  struct epoll_event ev;
  int nev = epoll_wait(serv->loop, &ev, 1, 0);
  if (nev <= 0) return nev;
  ev_cb_t* ev_cb = (ev_cb_t*)ev.data.ptr;
  ev_cb->handler(&ev);
  return nev;
}

void hs_add_events(http_request_t* request) {
  request->timer_handler = hs_request_timer_cb;

  // Watch for read events
  struct epoll_event ev;
  ev.events = EPOLLIN | EPOLLET;
  ev.data.ptr = request;
  epoll_ctl(request->server->loop, EPOLL_CTL_ADD, request->socket, &ev);

  // Add timer to timeout requests.
  int tfd = timerfd_create(CLOCK_MONOTONIC, 0);
  struct itimerspec ts = {};
  ts.it_value.tv_sec = 1;
  ts.it_interval.tv_sec = 1;
  timerfd_settime(tfd, 0, &ts, NULL);

  ev.events = EPOLLIN | EPOLLET;
  ev.data.ptr = &request->timer_handler;
  epoll_ctl(request->server->loop, EPOLL_CTL_ADD, tfd, &ev);
  request->timerfd = tfd;
}

void hs_add_write_event(http_request_t* request) {
  struct epoll_event ev;
  ev.events = EPOLLOUT | EPOLLET;
  ev.data.ptr = request;
  epoll_ctl(request->server->loop, EPOLL_CTL_MOD, request->socket, &ev);
}

#endif

#endif
#endif