cmake_minimum_required(VERSION 2.6.2)
project(quickmsg)

# policies
if (CMAKE_MAJOR_VERSION EQUAL 3)
  cmake_policy(SET CMP0042 OLD)
endif()

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
pkg_check_modules(LIB_JSONCPP "jsoncpp")
pkg_check_modules(LIB_GLOG "libglog")

# TBB formula is currently broken on Mac OS X; i.e. it doesn't install a pkgconfig spec
if (APPLE)
  execute_process(COMMAND brew --prefix tbb OUTPUT_VARIABLE TBB_PREFIX)
  if (NOT ${TBB_PREFIX} EQUAL "")
    include_directories(${TBB_PREFIX}/include)
  else()
    message("TBB not found. Please install using homebrew: 'brew install tbb'")
  endif()
else()
  pkg_check_modules(TBB "tbb")
endif()

find_package(Boost REQUIRED COMPONENTS date_time system thread log)
find_package(Java COMPONENTS Runtime Development)
find_package(JNI)

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
  src/quickmsg.cpp)
target_compile_options(${PROJECT_NAME} PRIVATE "-DBOOST_LOG_DYN_LINK")
# TODO: hmmm should tbb/zyre/czmq be in variables?
target_link_libraries(${PROJECT_NAME} tbb zyre czmq ${LIB_GLOG_LIBRARIES} ${Boost_LIBRARIES})

add_library("${PROJECT_NAME}-c" SHARED
  src/quickmsg_wrap.cpp
  src/publisher_wrap.cpp
  src/subscriber_wrap.cpp
  src/client_wrap.cpp
  src/service_wrap.cpp)
target_compile_options("${PROJECT_NAME}-c" PRIVATE "-DBOOST_LOG_DYN_LINK")
target_link_libraries("${PROJECT_NAME}-c" ${PROJECT_NAME})

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

# SWIG
# set(SWIG_EXECUTABLE "swig3.0")
find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})

find_package(PythonLibs 2)
include_directories(${PYTHON_INCLUDE_PATH})

set(CMAKE_SWIG_FLAGS "-threads")

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
swig_link_libraries(quickmsg_py ${PYTHON_LIBRARIES} ${PROJECT_NAME})

function(java_jar_native jar_file native_dir)
  set(JAVA_INSTALL_DIR "file://${CMAKE_INSTALL_PREFIX}/share/java")
  add_custom_command(TARGET quickmsg_java
    POST_BUILD
    COMMAND cmake -E echo "Creating native jar file... in ${native_dir}"
    COMMAND mkdir -p ${native_dir}
    COMMAND cp $<TARGET_FILE:quickmsg_java> ${native_dir}
    COMMAND cp $<TARGET_FILE:quickmsg> ${native_dir}
    COMMAND ${JAVA_ARCHIVE2} cMf ${jar_file} native)
  if (EXISTS ${MVN_BIN})
    add_custom_command(TARGET quickmsg_java
      POST_BUILD
      COMMAND cmake -E echo "Installing maven repo local/quickmsg etc in repo"
      COMMAND ${MVN_BIN} deploy:deploy-file -Durl=${JAVA_INSTALL_DIR} -DgroupId=jlowenz -DartifactId=quickmsg -Dversion=1.0 -Dpackaging=jar -Dfile=${CMAKE_CURRENT_BINARY_DIR}/quickmsg.jar
      COMMAND ${MVN_BIN} deploy:deploy-file -Durl=${JAVA_INSTALL_DIR} -DgroupId=jlowenz -DartifactId=quickmsg-native -Dversion=1.0 -Dpackaging=jar -Dfile=${CMAKE_CURRENT_BINARY_DIR}/quickmsg-native.jar)
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
  set(CMAKE_SWIG_FLAGS -package "quickmsg")

  swig_add_module(quickmsg_java java swig/quickmsg_java.i src/quickmsg_java.cpp)
  link_directories(/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64)
  swig_link_libraries(quickmsg_java ${JNI_LIBRARIES} ${PROJECT_NAME} ${Boost_LIBRARIES})
  # build the Java jars 
  set(CMAKE_JAVA_TARGET_OUTPUT_NAME quickmsg.jar)
  
  add_custom_command(TARGET quickmsg_java
    POST_BUILD
    COMMAND cmake -E echo "Compiling Java files..."
    #COMMAND ${CMAKE_COMMAND} make_directory ${CMAKE_CURRENT_BINARY_DIR}/quickmsg
    COMMAND cp ${PROJECT_SOURCE_DIR}/swig/quickmsg/*.java ${CMAKE_CURRENT_BINARY_DIR}/swig
    COMMAND ${JAVA_COMPILE2} ${CMAKE_CURRENT_BINARY_DIR}/swig/*.java -d ${CMAKE_CURRENT_BINARY_DIR}/
    COMMAND cmake -E echo "Creating jar file..."
    COMMAND ${JAVA_ARCHIVE2} cvf quickmsg.jar quickmsg)

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

# install Common Lisp components

# Add maven repository to clean list