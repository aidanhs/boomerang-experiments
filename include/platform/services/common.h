#ifndef SERVICES_H
#define SERVICES_H
#include <stdlib.h>
typedef struct LogParams
{
  const char * filename;
  size_t       line;
  const char * message;
} LogParams;

typedef struct ReportErrorParams
{
  const char * filename;
  size_t       line;
  const char * message;
} ReportErrorParams;

#endif
