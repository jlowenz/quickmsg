#ifndef JAVA_QUICKMSG_HPP
#define JAVA_QUICKMSG_HPP

#include <jni.h>

struct java_cb_data
{
  JNIEnv* env;
  jobject obj;
  bool init_thread;
  
  java_cb_data() : env(NULL), init_thread(false) {}
};




#endif
