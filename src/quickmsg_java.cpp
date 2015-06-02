void java_MessageCallback(const Message* msg, void* args)
{
  java_cb_data* data = static_cast<java_cb_data*>(args);
  const jclass cbIfaceClass = (data->env)->FindClass(data->env, "quickmsg/IMessageCallback");
  assert(cbIfaceClass);
  const jmethodID meth = data->env->GetMethodID(data->env, "handleMessage", "(quickmsg/Message)V");
}

const char* java_ServiceCallback(const Message* msg, void* args)
{

}
