cmake_minimum_required(VERSION 2.6.2)
project(quickmsg)

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
target_link_libraries(${PROJECT_NAME} tbb zyre)

add_executable(test_pub src/test_pub.cpp)
target_link_libraries(test_pub ${PROJECT_NAME})

add_executable(test_sub src/test_sub.cpp)
target_link_libraries(test_sub ${PROJECT_NAME})
