#include "lib.h"


int main()
{
  int ret;
  struct lib l;

  printf("Hello, Foo\n");

  ret = call_lib(&l);

  printf("Hello, lib: ret %d handle %d str %s\n", ret, l->handle, l->str);

  return 0;
}
