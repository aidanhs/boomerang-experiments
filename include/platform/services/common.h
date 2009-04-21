#ifndef SERVICES_H
#define SERVICES_H

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
