#ifndef __SERVICES_DISPATCHER_H__
#define __SERVICES_DISPATCHER_H__

template<class T>
class Dispatcher
{
public:
	static int32_t invoke(const char * serviceName, void * serviceParams);
};
class LoaderDispatcher : public Dispatcher<int>
{

};
#endif
