INCLUDE_DIRECTORIES(${MAIN_INCLUDE_DIR}/platform)

ADD_SUBDIRECTORY(plugin_framework)
ADD_SUBDIRECTORY(db)

SET(boomerang_platform_SRCS
   boomerang.cpp
   log.cpp
   platform_services.h
   loader_services.cpp
   loader_services.h
)


ADD_LIBRARY(boomerang_platform STATIC ${boomerang_platform_SRCS})

