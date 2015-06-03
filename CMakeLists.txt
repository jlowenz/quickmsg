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
pkg_check_modules(LIB_JSONCPP "jsoncpp")
pkg_check_modules(LIB_GLOG "libglog")

find_package(Boost REQUIRED COMPONENTS date_time system thread)
find_package(Java REQUIRED COMPONENTS Runtime Development)
find_package(JNI REQUIRED)

include_directories(${JNI_INCLUDE_DIRS})
include_directories(${LIB_ZYRE_INCLUDE_DIRS})
include_directories(${LIB_GLOG_INCLUDE_DIRS})

set(CMAKE_CXX_FLAGS "-std=c++11 -fPIC")

find_package(Doxygen)
if (DOXYGEN_FOUND)
  configure_file(${PROJECT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
  add_custom_target(doc ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile 
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} COMMENT "Generating API docs with Doxygen" VERBATIM)
endif()

# configure the PkgConfig file
configure_file(${PROJECT_SOURCE_DIR}/quickmsg.pc.in ${CMAKE_CURRENT_BINARY_DIR}/quickmsg.pc @ONLY)

add_library(${PROJECT_NAME} SHARED
  src/types.cpp
  src/group_node.cpp 
  src/publisher.cpp
  src/subscriber.cpp
  src/client.cpp
  src/service.cpp
  src/quickmsg.cpp
  src/quickmsg_wrap.cpp
  src/publisher_wrap.cpp
  src/subscriber_wrap.cpp
  src/client_wrap.cpp
  src/service_wrap.cpp)
# TODO: hmmm should tbb/zyre/czmq be in variables?
target_link_libraries(${PROJECT_NAME} tbb zyre czmq ${LIB_GLOG_LIBRARIES})

# SWIG
set(SWIG_EXECUTABLE "swig3.0")
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

set(SWIG_FILES swig/swig_interfaces.i swig/quickmsg_py.i swig/quickmsg_java.i)
swig_cpp("${SWIG_FILES}")

swig_add_module(quickmsg_py python swig/quickmsg_py.i)
swig_link_libraries(quickmsg_py ${PYTHON_LIBRARIES} ${PROJECT_NAME})


if (LIB_JSONCPP_FOUND)
  message("jsoncpp found, building service/client test")
  include_directories(${LIB_JSONCPP_INCLUDE_DIRS})

  add_executable(test_service test/cpp/test_service.cpp)
  target_link_libraries(test_service ${PROJECT_NAME} ${LIB_JSONCPP_LIBRARIES})

  add_executable(test_client test/cpp/test_client.cpp)
  target_link_libraries(test_client ${PROJECT_NAME} ${LIB_JSONCPP_LIBRARIES})
else()
  message("jsoncpp NOT found. Run 'apt-get install libjsoncpp-dev' to install")
endif()
  
add_executable(test_pub test/cpp/test_pub.cpp)
target_link_libraries(test_pub ${PROJECT_NAME})

add_executable(test_sub test/cpp/test_sub.cpp)
target_link_libraries(test_sub ${PROJECT_NAME})

SET(JAVA_RUNTIME2  $ENV{JAVA_HOME}/bin/java)
SET(JAVA_ARCHIVE2  $ENV{JAVA_HOME}/bin/jar)
SET(JAVA_COMPILE2  $ENV{JAVA_HOME}/bin/javac)

if (JAVA_FOUND)
  message("Java found. Running SWIG and building interfaces")
  include(UseJava)
  set(CMAKE_SWIG_FLAGS -package "quickmsg")
  file(GLOB CB_SRC ${PROJECT_SOURCE_DIR}/swig/quickmsg/*.java)
  file(COPY ${CB_SRC} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/swig)
  swig_add_module(quickmsg_java java swig/quickmsg_java.i src/quickmsg_java.cpp)
  swig_link_libraries(quickmsg_java ${JNI_LIBRARIES} ${PROJECT_NAME})
  # build the Java jars 
  set(CMAKE_JAVA_TARGET_OUTPUT_NAME quickmsg.jar)
  
  # replace the add_jar with custom java build commands
  #add_jar(quickmsg_jar
  #  #OUTPUT_NAME quickmsg
  #  SOURCES ${JAVA_SRC}
  #  VERSION ${PROJECT_VERSION})
  #add_dependencies(quickmsg_jar pre_jar quickmsg_java quickmsg)

  add_custom_command(TARGET quickmsg_java
    POST_BUILD
    COMMAND cmake -E echo "Compiling Java files..."
    COMMAND cmake -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/quickmsg
    COMMAND ${JAVA_COMPILE2} ${CMAKE_CURRENT_BINARY_DIR}/swig/*.java -d ${CMAKE_CURRENT_BINARY_DIR}/
    COMMAND cmake -E echo "Creating jar file..."
    COMMAND ${JAVA_ARCHIVE2} cvf quickmsg.jar quickmsg)

  # TODO: build the native jar as well
else()
  message("Java not found...")
endif()

# installation
install(TARGETS ${PROJECT_NAME} LIBRARY DESTINATION lib) # relative to ${CMAKE_INSTALL_PREFIX}

install(DIRECTORY include/${PROJECT_NAME}
  DESTINATION include
  PATTERN "*~" EXCLUDE)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/quickmsg.pc DESTINATION lib/pkgconfig)


# install Python components
install(FILES 
  ${CMAKE_CURRENT_BINARY_DIR}/_quickmsg_py.so
  ${CMAKE_CURRENT_BINARY_DIR}/swig/quickmsg_py.py
  DESTINATION lib/python2.7/dist-packages)

# install Java components

# install Common Lisp components