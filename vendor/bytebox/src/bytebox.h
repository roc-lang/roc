// C interface for bytebox wasm runtime.

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct bb_slice
{
	char* data;
	size_t length;
};
typedef struct bb_slice bb_slice;

enum bb_error
{
	BB_ERROR_OK,
	BB_ERROR_FAILED,
	BB_ERROR_OUTOFMEMORY,
	BB_ERROR_INVALIDPARAM,
	BB_ERROR_UNKNOWNEXPORT,
	BB_ERROR_UNLINKABLE_UNKNOWNIMPORT,
	BB_ERROR_UNLINKABLE_INCOMPATIBLEIMPORT,
	BB_ERROR_UNINSTANTIABLE_64BITLIMITSON32BITARCH,
	BB_ERROR_TRAP_DEBUG,
	BB_ERROR_TRAP_UNREACHABLE,
	BB_ERROR_TRAP_INTEGERDIVISIONBYZERO,
	BB_ERROR_TRAP_INTEGEROVERFLOW,
	BB_ERROR_TRAP_INDIRECTCALLTYPEMISMATCH,
	BB_ERROR_TRAP_INVALIDINTEGERCONVERSION,
	BB_ERROR_TRAP_OUTOFBOUNDSMEMORYACCESS,
	BB_ERROR_TRAP_UNDEFINEDELEMENT,
	BB_ERROR_TRAP_UNINITIALIZEDELEMENT,
	BB_ERROR_TRAP_OUTOFBOUNDSTABLEACCESS,
	BB_ERROR_TRAP_STACKEXHAUSTED,
};
typedef enum bb_error bb_error;

enum bb_valtype
{
	BB_VALTYPE_I32,
	BB_VALTYPE_I64,
	BB_VALTYPE_F32,
	BB_VALTYPE_F64,
};
typedef enum bb_valtype bb_valtype;

typedef float bb_v128[4];
union bb_val
{
	int32_t i32_val;
	int64_t i64_val;
	float f32_val;
	double f64_val;
	bb_v128 v128_val;
	uint32_t externref_val;
};
typedef union bb_val bb_val;

struct bb_module_definition_init_opts
{
	const char* debug_name;
};
typedef struct bb_module_definition_init_opts bb_module_definition_init_opts;

typedef struct bb_module_definition bb_module_definition;
typedef struct bb_module_instance bb_module_instance;
typedef struct bb_import_package bb_import_package;

typedef void bb_host_function(void* userdata, bb_module_instance* module, const bb_val* params, bb_val* returns);
struct bb_import_function
{
	bb_host_function* callback;
	void* userdata;
};
typedef struct bb_import_function bb_import_function;

typedef void* bb_wasm_memory_resize(void* mem, size_t new_size_bytes, size_t old_size_bytes, void* userdata);
typedef void bb_wasm_memory_free(void* mem, size_t size_bytes, void* userdata);

struct bb_wasm_memory_config
{
	bb_wasm_memory_resize* resize_callback;
	bb_wasm_memory_free* free_callback;
	void* userdata;
};
typedef struct bb_wasm_memory_config bb_wasm_memory_config;

struct bb_module_instance_instantiate_opts
{
	bb_import_package** packages;
	size_t num_packages;
	bb_wasm_memory_config wasm_memory_config;
	size_t stack_size;
	bool enable_debug;
};
typedef struct bb_module_instance_instantiate_opts bb_module_instance_instantiate_opts;

struct bb_module_instance_invoke_opts
{
	bool trap_on_start;
};
typedef struct bb_module_instance_invoke_opts bb_module_instance_invoke_opts;

struct bb_func_handle
{
	uint32_t index;
};
typedef struct bb_func_handle bb_func_handle;

struct bb_func_info
{
	bb_valtype* params;
	size_t num_params;
	bb_valtype* returns;
	size_t num_returns;
};
typedef struct bb_func_info bb_func_info;

enum bb_global_mut
{
	BB_GLOBAL_MUT_IMMUTABLE,
	BB_GLOBAL_MUT_MUTABLE,
};
typedef enum bb_global_mut bb_global_mut;

struct bb_global
{
	bb_val* value;
	bb_valtype type;
	bb_global_mut mut;
};
typedef struct bb_global bb_global;

enum bb_debug_trace_mode
{
	BB_DEBUG_TRACE_NONE,
	BB_DEBUG_TRACE_FUNCTION,
	BB_DEBUG_TRACE_INSTRUCTION,
};
typedef enum bb_debug_trace_mode bb_debug_trace_mode;

enum bb_debug_trap_mode
{
	BB_DEBUG_TRAP_MODE_DISABLED,
	BB_DEBUG_TRAP_MODE_ENABLED,
};
typedef enum bb_debug_trap_mode bb_debug_trap_mode;

const char* bb_error_str(bb_error err);

bb_module_definition* bb_module_definition_create(bb_module_definition_init_opts opts);
void bb_module_definition_destroy(bb_module_definition* definition);
bb_error bb_module_definition_decode(bb_module_definition* definition, const char* data, size_t length);
bb_slice bb_module_definition_get_custom_section(const bb_module_definition* definition, const char* name);

bb_import_package* bb_import_package_init(const char* name);
void bb_import_package_deinit(bb_import_package* package);	  // only deinit when all module_instances using the package have been destroyed
bb_error bb_import_package_add_function(bb_import_package* package, const char* export_name, const bb_valtype* params, size_t num_params, const bb_valtype* returns, size_t num_returns, bb_import_function* userdata);
bb_error bb_import_package_add_memory(bb_import_package* package, const bb_wasm_memory_config* config, const char* export_name, uint32_t min_pages, uint32_t max_pages);

void bb_set_debug_trace_mode(bb_debug_trace_mode mode);

bb_module_instance* bb_module_instance_create(bb_module_definition* definition);
void bb_module_instance_destroy(bb_module_instance* instance);
bb_error bb_module_instance_instantiate(bb_module_instance* instance, bb_module_instance_instantiate_opts opts);
bb_error bb_module_instance_find_func(bb_module_instance* instance, const char* func_name, bb_func_handle* out_handle);
bb_func_info bb_module_instance_func_info(bb_module_instance* instance, bb_func_handle handle);
bb_error bb_module_instance_invoke(bb_module_instance* instance, bb_func_handle, const bb_val* params, size_t num_params, bb_val* returns, size_t num_returns, bb_module_instance_invoke_opts opts);
bb_error bb_module_instance_resume(bb_module_instance* instance, bb_val* returns, size_t num_returns);
bb_error bb_module_instance_step(bb_module_instance* instance, bb_val* returns, size_t num_returns);
bb_error bb_module_instance_debug_set_trap(bb_module_instance* instance, uint32_t address, bb_debug_trap_mode trap_mode);
void* bb_module_instance_mem(bb_module_instance* instance, size_t offset, size_t length);
bb_slice bb_module_instance_mem_all(bb_module_instance* instance);
bb_error bb_module_instance_mem_grow(bb_module_instance* instance, size_t num_pages);
bb_error bb_module_instance_mem_grow_absolute(bb_module_instance* instance, size_t total_pages);
bb_global bb_module_instance_find_global(bb_module_instance* instance, const char* global_name);

bool bb_func_handle_isvalid(bb_func_handle handle);
