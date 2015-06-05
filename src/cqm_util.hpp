#include <quickmsg/types.hpp>
#include <quickmsg/ctypes.h>

using namespace quickmsg;

static inline qm_message_t to(const Message* m)
{
  return reinterpret_cast<qm_message_t>(m);
}
