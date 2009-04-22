SET(boomerang_cli_SOURCES driver.cpp)
ADD_EXECUTABLE(boomerang_cli ${boomerang_cli_SOURCES})
TARGET_LINK_LIBRARIES(boomerang_cli 
	#${BOOMERANG_LOADERS} 
	${BOOMERANG_FRONTENDS} 
	boomerang_db
	boomerang_type_solvers
	boomerang_transform
	boomerang_util
	boomerang_DSLs
	${BOOMERANG_CODE_GENERATORS}
	${boomerang_libs}
	${CMAKE_DL_LIBS}
	plugin_framework
	boomerang_platform
)

