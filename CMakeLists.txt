cmake_minimum_required(VERSION 2.8)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")

# workaround needed for bison parse.assert
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DYY_NULLPTR=nullptr")

project(ceraph)

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    add_compile_options(-Wall -Wextra -Werror -Wno-unused-parameter -pedantic)
endif()
if (MSVC)
    add_compile_options(/W4)
endif()

set(PROJECT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(SUBMODULE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)
set(GENERATOR_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/generators)

set(GENERATED_DEST_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)

set(CERAPH_VERSION_MAJOR 0)
set(CERAPH_VERSION_MINOR 0)
option(CERAPH_VERBOSE_DEBUGGING 0)
configure_file(
    ${GENERATOR_SOURCE_DIR}/ceraph_config.h.in
    ${GENERATED_DEST_DIR}/ceraph_config.h
    )
set(INCLUDE_DIRS ${INCLUDE_DIRS} ${GENERATED_DEST_DIR})

# file(GLOB_RECURSE SRC
#     "${PROJECT_SOURCE_DIR}/*.hpp"
#     "${PROJECT_SOURCE_DIR}/*.cpp"
#     "${PROJECT_SOURCE_DIR}/*.ipp"
# )

set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/ast.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/ast.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/codegen.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/codegen.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/main.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/operator.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/operator.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/scope.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/scope.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/sema.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/sema.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/type.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/type.cpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/util.hpp)
set(SRC ${SRC} ${PROJECT_SOURCE_DIR}/util.cpp)

find_package(LLVM 8 REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
set(INCLUDE_DIRS ${INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})
set(DEFINES ${DEFINES} ${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(LLVM_LIBS support core)
set(LINK_LIBS ${LINK_LIBS} ${LLVM_LIBS})

find_package(BISON 3 REQUIRED)
find_package(FLEX 2.6 REQUIRED)
set(YACC_INPUT_FILE ${GENERATOR_SOURCE_DIR}/parser.y)
set(LEX_INPUT_FILE ${GENERATOR_SOURCE_DIR}/lexer.l)
BISON_TARGET(Parser ${YACC_INPUT_FILE}
                ${GENERATED_DEST_DIR}/parser.cpp
                VERBOSE ${CMAKE_CURRENT_BINARY_DIR}/parser.output
                COMPILE_FLAGS -d)
FLEX_TARGET(Lexer ${LEX_INPUT_FILE} ${GENERATED_DEST_DIR}/lexer.cpp)
ADD_FLEX_BISON_DEPENDENCY(Lexer Parser)
set(INCLUDE_DIRS ${INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})
set(INCLUDE_DIRS ${INCLUDE_DIRS} ${PROJECT_SOURCE_DIR})
set(SRC ${SRC} ${BISON_Parser_OUTPUTS})
set(SRC ${SRC} ${FLEX_Lexer_OUTPUTS})
set_source_files_properties(${FLEX_Lexer_OUTPUTS} PROPERTIES COMPILE_FLAGS -Wno-error)
# include the source .y and .l files in xcodeproj
set(SRC ${SRC} ${YACC_INPUT_FILE} ${LEX_INPUT_FILE})

set(INCLUDE_DIRS ${INCLUDE_DIRS} ${SUBMODULE_DIR}/range-v3/include/)

add_definitions(${DEFINES})
add_executable(ceraph ${SRC})
target_link_libraries(ceraph ${LINK_LIBS})
include_directories(ceraph ${INCLUDE_DIRS})

enable_testing()
set(TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tests)

set(VERIFY_TEST_DIR ${TEST_DIR}/verify)
file(GLOB verify_tests ${VERIFY_TEST_DIR}/*.ce)
foreach(verify_test ${verify_tests})
    get_filename_component(test_name ${verify_test} NAME_WE)
    add_test(NAME ${test_name}
            COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/verify_llvm $<TARGET_FILE:ceraph> ${verify_test})
endforeach()
