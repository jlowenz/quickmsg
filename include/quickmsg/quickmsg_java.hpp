#ifndef JAVA_QUICKMSG_HPP
#define JAVA_QUICKMSG_HPP

struct java_cb_data
{
  JNIEnv* env;
  jobject obj;
};

void java_MessageCallback(const Message* msg, void* args);
const char* java_ServiceCallback(const Message* msg, void* args);

#endif
