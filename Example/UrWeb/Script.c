#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <urweb.h>

#include "Script.h"

uw_Basis_string   uw_Script_insert (uw_context ctx, uw_Basis_string mime, uw_Basis_string url)
{
  const char *pattern = "<script type=\"%s\" src=\"%s\"></script>";
  int len = strlen(mime) + strlen(url) + strlen(pattern);
  char *tag = uw_malloc(ctx, len);
  snprintf(tag, len, pattern, mime, url);
  return tag;
}

