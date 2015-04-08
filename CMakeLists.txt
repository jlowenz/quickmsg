cmake_minimum_required(VERSION 2.6.2)
project(quickmsg)

# directories
set (QUICKMSG_SRC_DIR src)
set (CMAKE_SWIG_OUTDIR swig)

# include directory
include_directories(${QUICKMSG_SRC_DIR})
include_directories(${PYTHON_INCLUDE_DIRS})
include_directories(include)

include(FindPkgConfig)
pkg_check_modules(LIB_ZMQ "libzmq")
pkg_check_modules(LIB_CZMQ "libczmq")
pkg_check_modules(LIB_ZYRE "libzyre")
pkg_check_modules(TBB "tbb")

find_package(Boost REQUIRED COMPONENTS date_time system thread)

include_directories(${LIB_ZYRE_INCLUDE_DIRS})

set(CMAKE_CXX_FLAGS "-std=c++11")

find_package(Doxygen)
if (DOXYGEN_FOUND)
  configure_file(${PROJECT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
  add_custom_target(doc ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile 
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} COMMENT "Generating API docs with Doxygen" VERBATIM)
endif()

add_library(${PROJECT_NAME} SHARED
  src/types.cpp
  src/group_node.cpp 
  src/publisher.cpp
  src/subscriber.cpp
  src/client.cpp
  src/service.cpp
  src/quickmsg.cpp)
target_link_libraries(${PROJECT_NAME} tbb zyre czmq json_spirit)

# SWIG
find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})

find_package(PythonLibs)
include_directories(${PYTHON_INCLUDE_PATH})


set(CMAKE_SWIG_FLAGS "")

# function for setting source properties
function(swig_cpp files)
  foreach(f ${files})
    message("swig src properties " ${f})
    set_source_files_properties(${f} PROPERTIES CPLUSPLUS ON)
#    set_source_files_properties(${f} PROPERTIES SWIG_FLAGS "-includeall")
  endforeach(f)
endfunction(swig_cpp)

set(SWIG_FILES swig/swig_interfaces.i
  swig/quickmsg.i)
swig_cpp("${SWIG_FILES}")
swig_add_module(quickmsg python swig/quickmsg.i)
swig_link_libraries(quickmsg ${PYTHON_LIBRARIES} ${PROJECT_NAME})

add_executable(test_pub src/test_pub.cpp)
target_link_libraries(test_pub ${PROJECT_NAME})

add_executable(test_sub src/test_sub.cpp)
target_link_libraries(test_sub ${PROJECT_NAME})

add_executable(test_service src/test_service.cpp)
target_link_libraries(test_service ${PROJECT_NAME})

add_executable(test_client src/test_client.cpp)
target_link_libraries(test_client ${PROJECT_NAME})


# TODO: Add install directives!