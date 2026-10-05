find_package (Threads)
if (CMAKE_SYSTEM_NAME STREQUAL FreeBSD OR CMAKE_SYSTEM_NAME STREQUAL Linux)
    find_package (Bfd)
endif()

include(TargetArch)
target_architecture(CMAKE_TARGET_ARCHITECTURES)
message(STATUS "Target architectures: ${CMAKE_TARGET_ARCHITECTURES}")

find_package (ZLIB REQUIRED)
find_package (CURL REQUIRED)

if (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)
set (DL_LIBRARY "")
else (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)
find_library (DL_LIBRARY NAMES dl)
if (DL_LIBRARY)
    message (STATUS "Found DL: ${DL_LIBRARY}")
else (DL_LIBRARY)
    message (FATAL_ERROR "Could NOT find DL")
endif (DL_LIBRARY)
endif (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)

if (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)
find_library (INTL_LIBRARY NAMES intl)
if (INTL_LIBRARY)
    message (STATUS "Found INTL: ${INTL_LIBRARY}")
else (INTL_LIBRARY)
    message (FATAL_ERROR "Could NOT find INTL")
endif (INTL_LIBRARY)
else (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)
set (INTL_LIBRARY "")
endif (CMAKE_SYSTEM_NAME STREQUAL FreeBSD)

find_library (M_LIBRARY NAMES m)
if (M_LIBRARY)
    message (STATUS "Found M: ${M_LIBRARY}")
else (M_LIBRARY)
    message (FATAL_ERROR "Could NOT find M")
endif (M_LIBRARY)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set (KCOV kcov)


# See http://eatmyrandom.blogspot.se/2010/06/automate-version-numbering-using-git.html
if (EXISTS "${CMAKE_SOURCE_DIR}/.git")
    find_program (GIT_EXECUTABLE NAMES git)
    if (GIT_EXECUTABLE)
        message (STATUS "Found Git: ${GIT_EXECUTABLE}")
    else (GIT_EXECUTABLE)
        message (FATAL_ERROR "Could NOT find Git")
    endif (GIT_EXECUTABLE)

    execute_process (COMMAND "${GIT_EXECUTABLE}"
            "--git-dir=${CMAKE_SOURCE_DIR}/.git"
            describe
            --abbrev=4
            HEAD
        OUTPUT_VARIABLE POD_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    message (STATUS "kcov version: ${POD_VERSION}")
else (EXISTS "${CMAKE_SOURCE_DIR}/.git")
    execute_process (COMMAND head
            -n 1
            "${CMAKE_SOURCE_DIR}/ChangeLog"
        COMMAND cut -d \( -f 2
        COMMAND cut -d \) -f 1
        OUTPUT_VARIABLE POD_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    message (STATUS "kcov version: ${POD_VERSION} (from changelog)")
endif (EXISTS "${CMAKE_SOURCE_DIR}/.git")

add_custom_command(
    OUTPUT version.c
    COMMAND "${CMAKE_COMMAND}"
           -E echo
           "const char *kcov_version = \\\"${POD_VERSION}\\\"\;"
        > version.c
)

set (SOLIB kcov_sowrapper)

set (${SOLIB}_SRCS
	solib-parser/phdr_data.c
	solib-parser/lib.c
	)

set (DISASSEMBLER_SRCS
    parsers/dummy-disassembler.cc
)

set (HAS_LIBBFD "0")
set (HAS_LIBBFD_DISASM_STYLED "0")

if (CMAKE_TARGET_ARCHITECTURES STREQUAL "i386" OR CMAKE_TARGET_ARCHITECTURES STREQUAL "x86_64")
	if (LIBBFD_FOUND)
		set (HAS_LIBBFD "1")
		set (DISASSEMBLER_SRCS
			parsers/bfd-disassembler.cc
		)
		set (DISASSEMBLER_LIBRARIES
			${LIBBFD_OPCODES_LIBRARY}
			${LIBBFD_BFD_LIBRARY}
			${LIBBFD_IBERTY_LIBRARY}
		)
		include(CheckCSourceCompiles)
		set(CMAKE_REQUIRED_LIBRARIES ${DISASSEMBLER_LIBRARIES})
		check_c_source_compiles("
		#define PACKAGE
		#define PACKAGE_VERSION
		#include <stdio.h>
		#include <dis-asm.h>

		int main(int argc, char **argv){
			struct disassemble_info info;
			init_disassemble_info(&info, stdout, NULL, NULL);
			return 0;
		}
		" TEST_LIBBFD_DISASM_STYLED)
		if (TEST_LIBBFD_DISASM_STYLED)
			set (HAS_LIBBFD_DISASM_STYLED "1")
		endif (TEST_LIBBFD_DISASM_STYLED)
	endif (LIBBFD_FOUND)
endif (CMAKE_TARGET_ARCHITECTURES STREQUAL "i386" OR CMAKE_TARGET_ARCHITECTURES STREQUAL "x86_64")

set (coveralls_SRCS writers/dummy-coveralls-writer.cc)

if (CURL_FOUND)
	set (coveralls_SRCS writers/coveralls-writer.cc)
endif (CURL_FOUND)

option (KCOV_STATIC_BUILD "Build a static binary" OFF)
mark_as_advanced (KCOV_STATIC_BUILD)

if (KCOV_STATIC_BUILD)
	message (STATUS "Building a static binary (no Coveralls support)")

	set (CURL_LIBRARIES "")
	# Coveralls doesn't work in a static setting
	set (coveralls_SRCS writers/dummy-coveralls-writer.cc)
    set (CMAKE_EXE_LINKER_FLAGS "-static")
else()
    find_package(OpenSSL REQUIRED)
    list( APPEND CURL_LIBRARIES ${OPENSSL_LIBRARIES} )
endif (KCOV_STATIC_BUILD)

set (ELF_SRCS
	dummy-solib-handler.cc
)
set (MACHO_SRCS
)
set (SOLIB_generated )

# Linux-specific sources
if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
	find_package (LibElf)
	find_package (ElfUtils)
	set (ELF_SRCS
		engines/clang-coverage-engine.cc
		engines/ptrace.cc
		engines/ptrace_linux.cc
		engines/kernel-engine.cc
		parsers/elf.cc
		parsers/elf-parser.cc
		parsers/dwarf.cc
		solib-handler.cc
		solib-parser/phdr_data.c
	)
	set (SOLIB_generated library.cc)
	add_library (${SOLIB} SHARED ${${SOLIB}_SRCS})
	set_target_properties(${SOLIB} PROPERTIES SUFFIX ".so")
	target_link_libraries(${SOLIB} dl)
elseif (CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
	find_package (LibElf)
	find_package (ElfUtils)
	set (ELF_SRCS
		engines/clang-coverage-engine.cc
		engines/ptrace.cc
		engines/ptrace_freebsd.cc
		parsers/elf.cc
		parsers/elf-parser.cc
		parsers/dwarf.cc
		solib-handler.cc
		solib-parser/phdr_data.c
	)
	set (SOLIB_generated library.cc)
	add_library (${SOLIB} SHARED ${${SOLIB}_SRCS})
	set_target_properties(${SOLIB} PROPERTIES SUFFIX ".so")
else() # Mac OSX
    find_package (dwarfutils REQUIRED)
    add_custom_command(
        OUTPUT
            ${CMAKE_CURRENT_BINARY_DIR}/mach_excServer.c
        MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/engines/osx/mach_exc.defs
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND mig ${CMAKE_CURRENT_SOURCE_DIR}/engines/osx/mach_exc.defs
        VERBATIM)

	# Not really true, but anyway
	set(LIBELF_FOUND 1)

	set (MACHO_SRCS
        parsers/macho-parser.cc
        engines/mach-engine.cc
        ${CMAKE_CURRENT_BINARY_DIR}/mach_excServer.c
    )

endif (CMAKE_SYSTEM_NAME STREQUAL "Linux")

set (${KCOV}_SRCS
    capabilities.cc
    collector.cc
    configuration.cc
    engine-factory.cc
    engines/bash-engine.cc
    engines/system-mode-engine.cc
    engines/system-mode-file-format.cc
    engines/python-engine.cc
    filter.cc
    gcov.cc
    main.cc
    merge-file-parser.cc
    output-handler.cc
    ${DISASSEMBLER_SRCS}
    parser-manager.cc
    reporter.cc
    source-file-cache.cc
    utils.cc
    writers/cobertura-writer.cc
    writers/codecov-writer.cc
    writers/json-writer.cc
    ${coveralls_SRCS}
    writers/html-writer.cc
    writers/sonarqube-xml-writer.cc
    writers/writer-base.cc
    writers/nocover.cc
    ${ELF_SRCS}
    ${MACHO_SRCS}
    include/capabilities.hh
    include/gcov.hh
    include/reporter.hh
    include/collector.hh
    include/generated-data-base.hh
    include/solib-handler.hh
    include/configuration.hh
    include/lineid.hh
    include/swap-endian.hh
    include/engine.hh
    include/manager.hh
    include/utils.hh
    include/file-parser.hh
    include/output-handler.hh
    include/writer.hh
    include/filter.hh
    include/phdr_data.h
    system-mode/file-data.cc
    )

set (KCOV_SYSTEM_MODE_SRCS
    configuration.cc
	dummy-solib-handler.cc
    engine-factory.cc
    engines/system-mode-file-format.cc
    engines/ptrace.cc
    engines/ptrace_linux.cc
    filter.cc
    gcov.cc
    include/capabilities.hh
    include/collector.hh
    include/configuration.hh
    include/engine.hh
    include/file-parser.hh
    include/filter.hh
    include/gcov.hh
    include/generated-data-base.hh
    include/lineid.hh
    include/manager.hh
    include/output-handler.hh
    include/phdr_data.h
    include/reporter.hh
    include/solib-handler.hh
    include/swap-endian.hh
    include/utils.hh
    include/writer.hh
    main-system-daemon.cc
    parser-manager.cc
	system-mode/file-data.cc
	system-mode/registration.cc
    utils.cc
)

set (KCOV_LIBRARY_PREFIX "/tmp")

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17 -g -Wall -D_GLIBCXX_USE_NANOSLEEP -DKCOV_LIBRARY_PREFIX=${KCOV_LIBRARY_PREFIX} -DKCOV_HAS_LIBBFD=${HAS_LIBBFD} -DKCOV_LIBFD_DISASM_STYLED=${HAS_LIBBFD_DISASM_STYLED}")

include_directories(
	include/
	${ZLIB_INCLUDE_DIR}
	)

if (LIBDW_FOUND)
    include_directories(${LIBDW_INCLUDE_DIRS})
endif (LIBDW_FOUND)

if (LIBELF_FOUND)
    include_directories(${LIBELF_INCLUDE_DIRS})
endif (LIBELF_FOUND)

if (CURL_FOUND)
    include_directories(${CURL_INCLUDE_DIRS})
endif (CURL_FOUND)

if (DWARFUTILS_FOUND)
    include_directories(${DWARFUTILS_INCLUDE_DIRS})
endif ()

add_library (bash_execve_redirector SHARED engines/bash-execve-redirector.c)
set_target_properties(bash_execve_redirector PROPERTIES SUFFIX ".so")
target_link_libraries(bash_execve_redirector ${DL_LIBRARY})

add_library (bash_tracefd_cloexec SHARED engines/bash-tracefd-cloexec.c)
set_target_properties(bash_tracefd_cloexec PROPERTIES SUFFIX ".so")
target_link_libraries(bash_tracefd_cloexec ${DL_LIBRARY})

add_library (kcov_system_lib SHARED engines/system-mode-binary-lib.cc utils.cc system-mode/registration.cc)
set_target_properties(kcov_system_lib PROPERTIES SUFFIX ".so")
target_link_libraries(kcov_system_lib
    "${DL_LIBRARY}"
    ${CURL_LIBRARIES}
    ${ZLIB_LIBRARIES}
)


add_custom_command(
    OUTPUT library.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
        $<TARGET_FILE:${SOLIB}>
            __library
        > library.cc
    DEPENDS
        ${SOLIB}
        "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

add_custom_command(
    OUTPUT bash-redirector-library.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
            $<TARGET_FILE:bash_execve_redirector>
            bash_redirector_library
        > bash-redirector-library.cc
    DEPENDS
        bash_execve_redirector
        "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

add_custom_command(
    OUTPUT bash-cloexec-library.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
            $<TARGET_FILE:bash_tracefd_cloexec>
            bash_cloexec_library
        > bash-cloexec-library.cc
    DEPENDS
        bash_tracefd_cloexec
        "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

add_custom_command(
    OUTPUT kcov-system-library.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
            $<TARGET_FILE:kcov_system_lib>
            kcov_system_library
        > kcov-system-library.cc
    DEPENDS
        kcov_system_lib
       "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

add_custom_command(
    OUTPUT python-helper.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
            "${CMAKE_CURRENT_SOURCE_DIR}/engines/python-helper.py"
            python_helper
        > python-helper.cc
    DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/engines/python-helper.py"
        "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

add_custom_command(
    OUTPUT bash-helper.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
            "${CMAKE_CURRENT_SOURCE_DIR}/engines/bash-helper.sh"
            bash_helper
            "${CMAKE_CURRENT_SOURCE_DIR}/engines/bash-helper-debug-trap.sh"
            bash_helper_debug_trap
        > bash-helper.cc
    DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/engines/bash-helper.sh"
        "${CMAKE_CURRENT_SOURCE_DIR}/engines/bash-helper-debug-trap.sh"
        "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

add_custom_command(
    OUTPUT html-data-files.cc
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/bcov.css" css_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/amber.png" icon_amber
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/glass.png" icon_glass
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/source-file.html" source_file_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/index.html" index_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/handlebars.js" handlebars_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/kcov.js" kcov_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/jquery.min.js" jquery_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/jquery.tablesorter.min.js" tablesorter_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/jquery.tablesorter.widgets.min.js" tablesorter_widgets_text
            "${CMAKE_CURRENT_SOURCE_DIR}/../data/tablesorter-theme.css" tablesorter_theme_text
        > html-data-files.cc
    DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/bcov.css"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/amber.png"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/glass.png"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/source-file.html"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/index.html"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/handlebars.js"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/kcov.js"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/jquery.min.js"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/jquery.tablesorter.min.js"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/js/jquery.tablesorter.widgets.min.js"
        "${CMAKE_CURRENT_SOURCE_DIR}/../data/tablesorter-theme.css"
        "${CMAKE_CURRENT_SOURCE_DIR}/bin-to-c-source.py"
)

# Reference: http://www.cmake.org/Wiki/CMake_RPATH_handling
if (SPECIFY_RPATH)
	set (CMAKE_SKIP_BUILD_RPATH OFF)
	set (CMAKE_BUILD_WITH_INSTALL_RPATH OFF)
	set (CMAKE_INSTALL_RPATH_USE_LINK_PATH ON)
	# the RPATH to be used when installing, but only if it's not a system directory
	list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" IS_SYSTEM_DIR)
	if (IS_SYSTEM_DIR EQUAL -1)
	    set (CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
	endif (IS_SYSTEM_DIR EQUAL -1)
endif (SPECIFY_RPATH)

if (LIBELF_FOUND)
	add_executable (${KCOV} ${${KCOV}_SRCS} ${SOLIB_generated} bash-redirector-library.cc bash-cloexec-library.cc python-helper.cc bash-helper.cc kcov-system-library.cc html-data-files.cc version.c)

	target_link_libraries(${KCOV}
		stdc++
		${CMAKE_THREAD_LIBS_INIT}
		CURL::libcurl
		"${DL_LIBRARY}"
		"${INTL_LIBRARY}"
		${DISASSEMBLER_LIBRARIES}
		${LIBDW_LIBRARIES}
		${LIBELF_LIBRARIES}
		${DWARFUTILS_LIBRARIES}
		"${M_LIBRARY}"
		${ZLIB_LIBRARIES}
	)
	install (TARGETS ${KCOV} DESTINATION "${KCOV_INSTALL_BINDIR}")
endif (LIBELF_FOUND)

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    add_executable (kcov-system-daemon ${KCOV_SYSTEM_MODE_SRCS} version.c)

    target_link_libraries(kcov-system-daemon
        stdc++
        ${CMAKE_THREAD_LIBS_INIT}
        "${DL_LIBRARY}"
        "${M_LIBRARY}"
        CURL::libcurl
        ${ZLIB_LIBRARIES}
	)
	install (TARGETS kcov-system-daemon DESTINATION "${KCOV_INSTALL_BINDIR}")
endif (CMAKE_SYSTEM_NAME STREQUAL "Linux")

# uninstall target
if(NOT TARGET uninstall)
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
        IMMEDIATE @ONLY)

    add_custom_target(uninstall
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()

# Sign OSX binaries (needed since it's a debugger)
if (APPLE)
    add_custom_command(TARGET ${KCOV} POST_BUILD VERBATIM
        COMMAND cmake -E echo Signing the kcov binary with an ad-hoc identity
        COMMAND codesign -s - --entitlements ${CMAKE_CURRENT_SOURCE_DIR}/../osx-entitlements.xml -f ${KCOV})
endif()
