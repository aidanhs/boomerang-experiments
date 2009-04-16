#include "base.h"
#include <boost/cerrno.hpp>
#include <boost/system/error_code.hpp>
using namespace boost;
namespace base
{
	std::string getErrorMessage()
	{
		char buff[1024]={};
		return std::string(buff);
	}
}


