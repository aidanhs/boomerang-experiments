
#ifndef LOG_H
#define LOG_H

#include "types.h"
#include <string>

class Statement;
class Exp;
class ExpressionSet;

class Log 
{
public:
    Log() { }
    virtual Log &operator<<(const char *str) = 0;
    virtual Log &operator<<(Statement *s);
    virtual Log &operator<<(Exp *e);
    virtual Log &operator<<(int i);
    virtual Log &operator<<(char c);
    virtual Log &operator<<(std::string& s);
    virtual Log &operator<<(double d);
    virtual Log &operator<<(ADDRESS a);
    virtual Log &operator<<(ExpressionSet *l);
    virtual ~Log() {};
};

#endif
