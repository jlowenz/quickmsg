#include <quickmsg/quickmsg_java.hpp>
#include <quickmsg/types.hpp>
#include <boost/shared_ptr.hpp>
#include <jni.h>
#include <cstring>

void java_MessageCallback(const quickmsg::Message* msg, void* args)
{
  java_cb_data* data = static_cast<java_cb_data*>(args);

  JavaVM* vms[3];
  jsize num_vms = 0;
  jint ret = JNI_GetCreatedJavaVMs(vms, 3, &num_vms);
  assert(ret == JNI_OK);
  assert(num_vms >= 1);
  vms[0]->AttachCurrentThreadAsDaemon((void**)&data->env, NULL);

  // get the callback interface method
  const jclass cbIfaceClass = (data->env)->
    FindClass("quickmsg/IMessageCallback");
  assert(cbIfaceClass);
  const jmethodID meth = (data->env)->
    GetMethodID(cbIfaceClass, 
		"handleMessage", "(Lquickmsg/Message;)V");
  assert(meth);

  // get the protected SWIG Message ctor
  const jclass jMessageCls = (data->env)->
    FindClass("quickmsg/Message");
  assert(jMessageCls);
  const jmethodID msgctor = (data->env)->
    GetMethodID(jMessageCls, "<init>", "(JZ)V");
  assert(msgctor);

  // create the shared_ptr ptr
  jlong jptr = 0;
  // why this??? why???
  *(quickmsg::Message**)&jptr = new quickmsg::Message(*msg);  
  jboolean ownMem = JNI_TRUE;
  // create/wrap the Message object
  jobject jmsg = (data->env)->NewObject(jMessageCls, msgctor,
					jptr, ownMem);

  //------------------------------------------------------------
  // call the callback interface method with the wrapped argument
  (data->env)->CallVoidMethod(data->obj, meth, jmsg);
  //------------------------------------------------------------
}

// almost exactly the same as above, but a different return type, and 
// different interface class
char* java_ServiceCallback(const quickmsg::Message* msg, void* args)
{
  java_cb_data* data = static_cast<java_cb_data*>(args);

  JavaVM* vms[3];
  jsize num_vms = 0;
  jint ret = JNI_GetCreatedJavaVMs(vms, 3, &num_vms);
  assert(ret == JNI_OK);
  assert(num_vms >= 1);
  vms[0]->AttachCurrentThreadAsDaemon((void**)&data->env, NULL);

  // get the callback interface method
  const jclass cbIfaceClass = (data->env)->
    FindClass("quickmsg/IServiceCallback");
  assert(cbIfaceClass);
  const jmethodID meth = (data->env)->
    GetMethodID(cbIfaceClass, 
		"handleMessage", "(Lquickmsg/Message;)Ljava/lang/String;");
  assert(meth);

  // get the protected SWIG Message ctor
  const jclass jMessageCls = (data->env)->
    FindClass("quickmsg/Message");
  assert(jMessageCls);
  const jmethodID msgctor = (data->env)->
    GetMethodID(jMessageCls, "<init>", "(JZ)V");
  assert(msgctor);

  // create the shared_ptr ptr
  jlong jptr = 0;
  *(quickmsg::Message**)&jptr = new quickmsg::Message(*msg);
  jboolean ownMem = JNI_TRUE;
  // create/wrap the Message object
  jobject jmsg = (data->env)->NewObject(jMessageCls, msgctor,
					 jptr, ownMem);
  //------------------------------------------------------------
  // call the callback interface method with the wrapped argument
  jstring ret_str = (jstring)(data->env)->CallObjectMethod(data->obj, meth, jmsg);
  //------------------------------------------------------------
  
  // parse the string, extract the data (don't leak memory in the JVM)
  jboolean is_copy;
  const char* java_chars = (data->env)->
    GetStringUTFChars(ret_str, &is_copy);
  // need to copy the string to the C++ heap
  jsize len = (data->env)->GetStringUTFLength(ret_str);
  char* ret_copy = (char*)malloc(sizeof(char)*(len+1)); // THIS NEEDS TO BE DELETED BY CALLER
  strncpy(ret_copy, java_chars, len);
  ret_copy[len] = '\0';
  (data->env)->ReleaseStringUTFChars(ret_str, java_chars);
  return ret_copy;
}
