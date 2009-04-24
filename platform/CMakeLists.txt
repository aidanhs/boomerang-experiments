# common includes
INCLUDE_DIRECTORIES(${MAIN_INCLUDE_DIRECTORY}/platform)
#platform only includes
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/include)

SET(boomerang_platform_INCLUDES_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

ADD_SUBDIRECTORY(plugin_framework)
ADD_SUBDIRECTORY(db)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/include/db)
SET(boomerang_platform_services_SRCS
   services/common.cpp
   services/loader.cpp
)
SET(boomerang_platform_services_INCLUDE
   services/dispatcher.h
   ${MAIN_INCLUDE_DIRECTORY}/platform/services/common.h
   ${MAIN_INCLUDE_DIRECTORY}/platform/services/loader.h
)

SET(boomerang_platform_SRCS
	boomerang.cpp
	log.cpp
	${boomerang_platform_services_SRCS}
	${boomerang_platform_services_INCLUDE}
)
# separating the various subsystems into 'subfolders' on MSVC
IF(MSVC)
  SOURCE_GROUP("Services\\Header Files" FILES ${boomerang_platform_services_INCLUDE})
  SOURCE_GROUP("Services\\Source Files" FILES ${boomerang_platform_services_SRCS})
ENDIF(MSVC)
ADD_LIBRARY(boomerang_platform STATIC ${boomerang_platform_SRCS})

