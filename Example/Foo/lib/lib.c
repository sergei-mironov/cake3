
#include "lib.h"

int call_lib(struct lib* l)
{
  l->handle = 33;
  l->str = "hello lib";
  return 0;
}
