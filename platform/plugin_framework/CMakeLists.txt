SET(plugin_framework_SOURCES
    base.cpp
    DynamicLibrary.cpp
    PluginManager.cpp
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/base.h
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/DynamicLibrary.h
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/PluginManager.h
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/PluginHelper.h
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/StreamingException.h
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/plugin.h
    ${MAIN_INCLUDE_DIRECTORY}/platform/plugin_framework/ObjectAdapter.h
)
IF(APPLE)
	ADD_DEFINITIONS(-DPF_PLATFORM_MAC)
ELSEIF(WIN32 OR CYGWIN)
	ADD_DEFINITIONS(-DPF_PLATFORM_WINDOWS)
ELSEIF(UNIX)
	ADD_DEFINITIONS(-DPF_PLATFORM_LINUX)
ENDIF(APPLE)

ADD_LIBRARY(plugin_framework STATIC ${plugin_framework_SOURCES})
