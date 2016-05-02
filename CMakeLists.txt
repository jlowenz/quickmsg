cmake_minimum_required(VERSION 2.6.2)
project(quickmsg)

# policies
if (CMAKE_MAJOR_VERSION EQUAL 3)
  cmake_policy(SET CMP0042 OLD)
endif()

# Windows build type
if (WIN32)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Specify build configuration")
endif()

# directories
set (QUICKMSG_SRC_DIR src)
set (CMAKE_SWIG_OUTDIR swig)

# include directory
include_directories(${QUICKMSG_SRC_DIR})
include_directories(${PYTHON_INCLUDE_DIRS})
include_directories(include)

# find cross-platform standard components
if (WIN32)
  # Default mode for zeromq+friends is /MT and /MTd (not /MD which is the default for Boost)
  set(Boost_USE_STATIC_LIBS ON)
  set(Boost_USE_STATIC_RUNTIME ON)
endif()
find_package(Boost REQUIRED COMPONENTS date_time system thread log coroutine)
include_directories(${Boost_INCLUDE_DIRS})

find_package(Java COMPONENTS Runtime Development)
find_package(JNI)

# SWIG
set(SWIG_EXECUTABLE "swig" CACHE STRING "Name of the SWIG executable")
find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})

find_package(PythonLibs 2)
include_directories(${PYTHON_INCLUDE_PATH})

if (!WIN32) 
  include(FindPkgConfig)
  pkg_check_modules(LIB_ZMQ "libzmq")
  pkg_check_modules(LIB_CZMQ "libczmq")
  pkg_check_modules(LIB_ZYRE "libzyre")
  pkg_check_modules(LIB_JSONCPP "jsoncpp")
  find_package(TBB REQUIRED)
  include_directories(${TBB_INCLUDE_DIRS})

  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -fPIC")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-long-long -pedantic")
  set(CMAKE_C_FLAGS "-fPIC -Wall -Wno-long-long -pedantic")
else()
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Ot /W3 /MT")
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Ot /W3 /MT")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Od /W3 /MTd")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /W3 /MTd")
  add_definitions("-D_WINVER=0x0601")
  add_definitions("-D_WIN32_WINNT=0x0601")
  add_definitions("-D_SCL_SECURE_NO_WARNINGS -DLIBZMQ_STATIC -DLIBCZMQ_STATIC -DLIBZYRE_STATIC")
  file(MAKE_DIRECTORY "${PROJECT_SOURCE_DIR}/bin")
  set(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/bin/" CACHE PATH "Install prefix" FORCE)
  set(WIN_PLATFORM "x64" CACHE STRING "Windows Architecture [x64,Win32]")
  if (${CMAKE_BUILD_TYPE} MATCHES "Rel.*")
    set(WIN_MODE "Release" CACHE STRING "Windows Configuration Mode [Release,Debug]")
  else()
    set(WIN_MODE "Debug" CACHE STRING "Windows Configuration Mode [Release,Debug]")
  endif()
  message("-- BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
  message("-- WIN_MODE:   ${WIN_MODE}")
  set(WIN_VS_VERSION "v120" CACHE STRING "Visual Studio Version [v120]")
  set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})
  set(TBB_ROOT_DIR "${PROJECT_SOURCE_DIR}/../tbb" CACHE PATH "Root directory for Threading Building Blocks")
  set(TBB_LIBRARY "${TBB_ROOT_DIR}/lib/intel64/vc12/")
  set(PC_LIBZMQ_LIBRARY_HINTS
    "${PROJECT_SOURCE_DIR}/../libzmq/bin/${WIN_PLATFORM}/${WIN_MODE}/${WIN_VS_VERSION}/static/")
  set(PC_LIBZMQ_INCLUDE_HINTS
    "${PROJECT_SOURCE_DIR}/../libzmq/include")
  set(PC_CZMQ_LIBRARY_HINTS
    "${PROJECT_SOURCE_DIR}/../czmq/bin/${WIN_PLATFORM}/${WIN_MODE}/${WIN_VS_VERSION}/static/")
  set(PC_CZMQ_INCLUDE_HINTS
    "${PROJECT_SOURCE_DIR}/../czmq/include")
  set(PC_ZYRE_LIBRARY_HINTS
    "${PROJECT_SOURCE_DIR}/../zyre/bin/${WIN_PLATFORM}/${WIN_MODE}/${WIN_VS_VERSION}/static/")
  set(PC_ZYRE_INCLUDE_HINTS
    "${PROJECT_SOURCE_DIR}/../zyre/include")
  
  find_package(libzmq)
  find_package(czmq)
  find_package(zyre)

  set(LIBZMQ_LIBRARIES "${LIBZMQ_LIBRARIES};Advapi32.lib;Ws2_32.lib;Rpcrt4.lib;Iphlpapi.lib")
  
  find_package(TBB REQUIRED)
  include_directories(${TBB_INCLUDE_DIRS})

  # Switching on BUILD_TYPE is ineffective for "multi-configuration" generators like MSVC!
endif()

include_directories(${LIBZMQ_INCLUDE_DIRS})
include_directories(${CZMQ_INCLUDE_DIRS})
include_directories(${ZYRE_INCLUDE_DIRS})

# TBB formula is currently broken on Mac OS X; i.e. it doesn't install a pkgconfig spec
if (APPLE)
  execute_process(COMMAND brew --prefix tbb OUTPUT_VARIABLE TBB_PREFIX)
  if (NOT ${TBB_PREFIX} EQUAL "")
    include_directories(${TBB_PREFIX}/include)
  else()
    message("TBB not found. Please install using homebrew: 'brew install tbb'")
  endif()
else()
endif()

include_directories(${JNI_INCLUDE_DIRS})
include_directories(${LIB_ZYRE_INCLUDE_DIRS})


find_package(Doxygen)
if (DOXYGEN_FOUND)
  configure_file(${PROJECT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
  add_custom_target(doc ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile 
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} COMMENT "Generating API docs with Doxygen" VERBATIM)
endif()

# configure the PkgConfig file
configure_file(${PROJECT_SOURCE_DIR}/quickmsg.pc.in ${CMAKE_CURRENT_BINARY_DIR}/quickmsg.pc @ONLY)

################################################################################
# Set some useful collection variables in one place
set(QM_CPP_FILES
  src/types.cpp
  src/group_node.cpp 
  src/publisher.cpp
  src/subscriber.cpp
  src/clock_sync.cpp
  src/client.cpp
  src/service.cpp
  src/quickmsg.cpp)
set(QM_C_FILES
  src/quickmsg_wrap.cpp
  src/publisher_wrap.cpp
  src/subscriber_wrap.cpp
  src/client_wrap.cpp
  src/service_wrap.cpp)

# we need to fix the other library paths
set(QM_DEPENDS_LIBS
  optimized 
  ${ZYRE_LIBRARIES}
  ${CZMQ_LIBRARIES}
  ${LIBZMQ_LIBRARIES}
  ${Boost_LIBRARIES}
  ${TBB_tbb_LIBRARY_RELEASE}
  debug
  ${ZYRE_LIBRARIES}
  ${CZMQ_LIBRARIES}
  ${LIBZMQ_LIBRARIES}
  ${Boost_LIBRARIES_DEBUG}
  ${TBB_tbb_LIBRARY_DEBUG})

################################################################################
# BUILD THE C++ library
if (!WIN32)
  add_library(${PROJECT_NAME} SHARED ${QM_CPP_FILES})
  target_compile_options(${PROJECT_NAME} PRIVATE "-DBOOST_LOG_DYN_LINK")
else()
  # Windows cannot easily generate DLLs with C++ code
  add_library(${PROJECT_NAME} STATIC ${QM_CPP_FILES})
endif()
target_link_libraries(${PROJECT_NAME} ${QM_DEPENDS_LIBS})

################################################################################
# Clock executables (for testing)
add_executable(clk_service_test src/clk_service_test.cpp)
target_link_libraries(clk_service_test ${PROJECT_NAME})
add_executable(clk_client_test src/clk_client_test.cpp)
target_link_libraries(clk_client_test ${PROJECT_NAME})

################################################################################
# Build the C Library
add_library("${PROJECT_NAME}-c" SHARED ${QM_CPP_FILES} ${QM_C_FILES})
if (!WIN32)
  target_compile_options("${PROJECT_NAME}-c" PRIVATE "-DBOOST_LOG_DYN_LINK")
endif()
target_link_libraries("${PROJECT_NAME}-c" ${QM_DEPENDS_LIBS})

################################################################################
# Test Executables
# ------------------------------------------------------------------------------

if (LIB_JSONCPP_FOUND)
  message("-- jsoncpp found, building service/client test")
  include_directories(${LIB_JSONCPP_INCLUDE_DIRS})
  
  add_executable(test_service test/cpp/test_service.cpp)
  target_link_libraries(test_service ${PROJECT_NAME} ${LIB_JSONCPP_LIBRARIES})
  
  add_executable(test_client test/cpp/test_client.cpp)
  target_link_libraries(test_client ${PROJECT_NAME} ${LIB_JSONCPP_LIBRARIES})
else()
  message("-- jsoncpp NOT found. Run 'apt-get install libjsoncpp-dev' or 'git clone https://github.com/open-source-parsers/jsoncpp' to install")
  message("-- C++ service test code will not be built.")
  message("-- This is NOT an error, you only need jsoncpp to build C++ test code")
endif()

add_executable(test_pub test/cpp/test_pub.cpp)
target_link_libraries(test_pub ${PROJECT_NAME})

add_executable(test_sub test/cpp/test_sub.cpp)
target_link_libraries(test_sub ${PROJECT_NAME})

add_executable(test_node test/cpp/test_node.cpp)
target_link_libraries(test_node ${PROJECT_NAME})

add_executable(test_cpub test/c/test_pub.c)
target_link_libraries(test_cpub "${PROJECT_NAME}-c")

add_executable(test_csub test/c/test_sub.c)
target_link_libraries(test_csub "${PROJECT_NAME}-c")

add_executable(test_cclient test/c/test_client.c)
target_link_libraries(test_cclient "${PROJECT_NAME}-c")

add_executable(test_cservice test/c/test_service.c)
target_link_libraries(test_cservice "${PROJECT_NAME}-c")

################################################################################
# External Language Extensions
set(CMAKE_SWIG_FLAGS "-threads" "-builtin")

# function for setting source properties
function(swig_cpp files)
  foreach(f ${files})
    message("swig src properties " ${f})
    set_source_files_properties(${f} PROPERTIES CPLUSPLUS ON)
    #    set_source_files_properties(${f} PROPERTIES SWIG_FLAGS "-includeall")
  endforeach(f)
endfunction(swig_cpp)

# function to clean the swig directory, to try and force rebuild
# add_custom_target(clean_swig ALL
#   COMMAND ${CMAKE_COMMAND} remove_directory ${CMAKE_CURRENT_BINARY_DIR}/swig
#   DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/swig)

set(SWIG_FILES swig/swig_interfaces.i swig/quickmsg_py.i swig/quickmsg_java.i)
swig_cpp("${SWIG_FILES}")

swig_add_module(quickmsg_py python swig/quickmsg_py.i)
swig_link_libraries(quickmsg_py ${PYTHON_LIBRARIES} ${PROJECT_NAME} ${QM_DEPENDS_LIBS})

function(java_jar_native jar_file native_dir)
  set(JAVA_INSTALL_DIR "file://${CMAKE_INSTALL_PREFIX}/share/java")
  add_custom_command(TARGET quickmsg_java
    POST_BUILD
    COMMAND cmake -E echo "Creating native jar file... (${jar_file}) in ${native_dir}"
    COMMAND cmake -E make_directory ${native_dir}
    COMMAND cmake -E copy $<TARGET_FILE:quickmsg_java> ${native_dir}
    #COMMAND cmake -E copy $<TARGET_FILE:quickmsg> ${native_dir}
    COMMAND ${JAVA_ARCHIVE2} cvMf ${jar_file} native)
  if (EXISTS ${MVN_BIN})
    add_custom_command(TARGET quickmsg_java
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E echo "Installing maven repo local/quickmsg etc in repo"
      COMMAND call ${MVN_BIN} deploy:deploy-file -Durl=${JAVA_INSTALL_DIR} -DgroupId=jlowenz -DartifactId=quickmsg -Dversion=1.0 -Dpackaging=jar -Dfile=${CMAKE_CURRENT_BINARY_DIR}/quickmsg.jar)
    add_custom_command(TARGET quickmsg_java
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E echo "Installing maven repo local/quickmsg etc in repo WTF"
      COMMAND call ${MVN_BIN} deploy:deploy-file -Durl=${JAVA_INSTALL_DIR} -DgroupId=jlowenz -DartifactId=quickmsg-native -Dversion=1.0 -Dpackaging=jar -Dfile=${CMAKE_CURRENT_BINARY_DIR}/quickmsg-native.jar)
    # Add local maven repo to clean list if it exists
    set(M2_HOME_REPO "$ENV{M2_HOME}/repository/jlowenz")
    if(EXISTS ${M2_HOME_REPO})
      message("Adding maven repo ${M2_HOME_REPO} to list for make clean")
      set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${M2_HOME_REPO})
    else ()
      set(M2_DEFAULT_REPO "$ENV{HOME}/.m2/repository/jlowenz")
      if (EXISTS ${M2_DEFAULT_REPO})
        message("Adding maven repo ${M2_DEFAULT_REPO} to list for make clean")
        set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${M2_DEFAULT_REPO})
      else ()
        message("Not adding local maven quickmsg repository to make clean list")
      endif()
    endif ()
  else ()
    message("Not installing to Maven repo")
  endif ()
endfunction(java_jar_native)

if (JAVA_FOUND AND JNI_FOUND)
  message("Java found. Running SWIG and building interfaces")
  #SET(JAVA_RUNTIME2  $ENV{JAVA_HOME}/bin/java)
  #SET(JAVA_ARCHIVE2  $ENV{JAVA_HOME}/bin/jar)
  #SET(JAVA_COMPILE2  $ENV{JAVA_HOME}/bin/javac)
  # HRRM - fix this junk up
  SET(JAVA_RUNTIME2  ${Java_JAVA_EXECUTABLE})
  SET(JAVA_ARCHIVE2  ${Java_JAR_EXECUTABLE})
  SET(JAVA_COMPILE2  ${Java_JAVAC_EXECUTABLE})
  # check to see if maven is installed
  find_program(MVN_BIN NAMES mvn PATHS /usr/bin)
  message("Maven: ${MVN_BIN}")

  include(UseJava)
  set(CMAKE_SWIG_FLAGS "-package" "quickmsg")

  swig_add_module(quickmsg_java java swig/quickmsg_java.i src/quickmsg_java.cpp)
  #link_directories(/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64)
  swig_link_libraries(quickmsg_java ${JNI_LIBRARIES} ${PROJECT_NAME} ${QM_DEPENDS_LIBS})
  # build the Java jars 
  set(CMAKE_JAVA_TARGET_OUTPUT_NAME quickmsg.jar)

  #file(TO_NATIVE_PATH ${PROJECT_SOURCE_DIR}/swig/quickmsg/ JAVA_SRC_DIR)
  #file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR}/swig SWIG_OUT_DIR)
  # THIS IS RIDICULOUS: we should not have to do this!
  file(GLOB PRE_JAVA_SOURCE ${PROJECT_SOURCE_DIR}/swig/quickmsg/*.java)
  add_custom_command(TARGET quickmsg_java
    POST_BUILD
    COMMAND cmake -E echo "Compiling Java files..."
    #COMMAND ${CMAKE_COMMAND} make_directory ${CMAKE_CURRENT_BINARY_DIR}/quickmsg
    #COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/swig/quickmsg
    COMMAND ${CMAKE_COMMAND} -E copy ${PRE_JAVA_SOURCE} ${CMAKE_CURRENT_BINARY_DIR}/swig
    #COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND "${JAVA_COMPILE2}" ${CMAKE_CURRENT_BINARY_DIR}/swig/*.java -d ${CMAKE_CURRENT_BINARY_DIR}/
    COMMAND cmake -E echo "Creating jar file..."
    COMMAND "${JAVA_ARCHIVE2}" cvf quickmsg.jar quickmsg)

  set(JAVA_NATIVE_JAR ${CMAKE_CURRENT_BINARY_DIR}/quickmsg-native.jar)
  if (UNIX)
    set(JAVA_NATIVE_DIR native/linux/x86_64)
    java_jar_native(${JAVA_NATIVE_JAR} ${JAVA_NATIVE_DIR})
    # add_custom_command(TARGET quickmsg_java
    #   POST_BUILD
    #   COMMAND cmake -E echo "Creating native jar file..."
    #   COMMAND mkdir -p ${JAVA_NATIVE_DIR}
    #   COMMAND cp $<TARGET_FILE:quickmsg_java> ${JAVA_NATIVE_DIR}
    #   COMMAND cp $<TARGET_FILE:quickmsg> ${JAVA_NATIVE_DIR}
    #   COMMAND ${JAVA_ARCHIVE2} cMf ${JAVA_NATIVE_JAR} ${CMAKE_CURRENT_BINARY_DIR}/native)
  elseif (WIN32)
    set(JAVA_NATIVE_DIR native/windows/x86_64)
    java_jar_native(${JAVA_NATIVE_JAR} ${JAVA_NATIVE_DIR})
    # add_custom_command(TARGET quickmsg_java
    #   POST_BUILD
    #   COMMAND cmake -E echo "Creating native jar file..."
    #   COMMAND mkdir -p ${JAVA_NATIVE_DIR}
    #   COMMAND cp $<TARGET_FILE:quickmsg_java> ${JAVA_NATIVE_DIR}
    #   COMMAND cp $<TARGET_FILE:quickmsg> ${JAVA_NATIVE_DIR}
    #   COMMAND ${JAVA_ARCHIVE2} cMf ${JAVA_NATIVE_JAR} ${CMAKE_CURRENT_BINARY_DIR}/native)    
  elseif (APPLE)
    set(JAVA_NATIVE_DIR native/macosx/x86_64)
    java_jar_native(${JAVA_NATIVE_JAR} ${JAVA_NATIVE_DIR})
  endif()

  # TODO: build the native jar as well
else()
  message("Java not found...")
endif()

if (!WIN32) 
  # installation
  install(TARGETS ${PROJECT_NAME} "${PROJECT_NAME}-c" "${PROJECT_NAME}_java" LIBRARY DESTINATION lib) # relative to ${CMAKE_INSTALL_PREFIX}

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
  install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/quickmsg.jar
    ${CMAKE_CURRENT_BINARY_DIR}/quickmsg-native.jar
    DESTINATION share/java)
endif()

# install Common Lisp components

# Add maven repository to clean list
