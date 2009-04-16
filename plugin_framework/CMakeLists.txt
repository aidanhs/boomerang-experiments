SET(plugin_framework_SOURCES
    base.cpp
    DynamicLibrary.cpp
    PluginManager.cpp
    base.h
    DynamicLibrary.h
    PluginManager.h
    PluginHelper.h
    StreamingException.h
    plugin.h
    ObjectAdapter.h
)
IF(APPLE)
	ADD_DEFINITIONS(-DPF_PLATFORM_MAC)
ELSEIF(WIN32 OR CYGWIN)
	ADD_DEFINITIONS(-DPF_PLATFORM_WINDOWS)
ELSEIF(UNIX)
	ADD_DEFINITIONS(-DPF_PLATFORM_LINUX)
ENDIF(APPLE)

ADD_LIBRARY(plugin_framework STATIC ${plugin_framework_SOURCES})
