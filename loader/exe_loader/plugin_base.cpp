#include "plugin_base.h"
#include "plugin_framework/plugin.h"
#include "plugin_framework/PluginHelper.h" 
#include "ExeBinaryFile.h"
extern "C" PLUGIN_API int32_t ExitFunc()
{
	return 0;
}

extern "C" PLUGIN_API PF_ExitFunc PF_initPlugin(const PF_PlatformServices * params)
{
	PluginHelper p(params); 
	p.registerObject< LoaderWrapper<ExeBinaryFile,ILoader> >((const char *)"ExeBinaryFile"); 
	return p.getResult();
}


