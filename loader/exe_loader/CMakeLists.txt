IF(NOT ExeBinaryFile_LOADER_VISITED)
	# add the loader as a dll
	ADD_LIBRARY(ExeBinaryFile SHARED 
				ExeBinaryFile.cpp
				plugin_base.cpp
				ExeBinaryFile.h
				plugin_base.h
				)
	# add this loader to project's global loader list
	LIST(APPEND _BOOMERANG_LOADERS ExeBinaryFile)
	SET(ExeBinaryFile_LOADER_VISITED true)
ENDIF(NOT ExeBinaryFile_LOADER_VISITED)
