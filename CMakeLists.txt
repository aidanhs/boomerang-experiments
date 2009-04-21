PROJECT(Boomerang)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
# this will set th

SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake_scripts;${CMAKE_MODULE_PATH})

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/compiled)
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/compiled)
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/compiled)

OPTION(BUILD_TESTING "Build the testing tree." OFF)
IF(BUILD_TESTING)
	ENABLE_TESTING()
ENDIF(BUILD_TESTING)

SET(VERSION "0.3.1")
INCLUDE(BOOMERANG_Configure)
INCLUDE(BOOMERANG_Macros)

# The BOOMERANG_FRONTENDS will be filled ba appropriate ADD_FRONTEND macros
SET(BOOMERANG_FRONTENDS "" CACHE INTERNAL "")
SET(BOOMERANG_LOADERS "" CACHE INTERNAL "")
SET(BOOMERANG_CODE_GENERATORS "" CACHE INTERNAL "")
IF(MSVC)
	ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
ENDIF(MSVC)
# this is the list of libraries that will be extended
# by various configured libraries
SET(boomerang_depends_on "")


SET(Boost_USE_MULTITHREADED ON)
find_package( Boost 1.35.0 REQUIRED COMPONENTS filesystem )
FIND_PACKAGE(Boost)
LINK_DIRECTORIES(${Boost_LIBRARY_DIRS})
INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/include)
IF(USE_GC)
	FIND_PACKAGE(BoehmGC REQUIRED)
	SET(boomerang_libs ${boomerang_libs} ${BOEHMGC_LIBRARIES})
	INCLUDE_DIRECTORIES(${BOEHMGC_INCLUDE_DIRS})
	ADD_DEFINITIONS(${BOEHMGC_DEFINITIONS})
ELSE(USE_GC)
	ADD_DEFINITIONS(-DNO_GARBAGE_COLLECTOR)
ENDIF(USE_GC)

INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR})
SET(MAIN_INCLUDE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

ADD_SUBDIRECTORY(loader)
ADD_SUBDIRECTORY(c)
ADD_SUBDIRECTORY(codegen)
ADD_SUBDIRECTORY(type)
ADD_SUBDIRECTORY(transform)
ADD_SUBDIRECTORY(util)
ADD_SUBDIRECTORY(frontend)
ADD_SUBDIRECTORY(platform)
ADD_SUBDIRECTORY(ui)

INSTALL(FILES include/*.h DESTINATION include)

# this is put at the end so that first cmake configure will assume USE_GC NO, 
# and will not check for existence of Boehm GC
OPTION(USE_GC "Use Boehm garbage collector, only developers should turn this off" true)

IF(BUILD_TESTING)
	FIND_PACKAGE(CppUnit REQUIRED)
	INCLUDE_DIRECTORIES(${CPPUNIT_INCLUDE_DIR})

	ENABLE_TESTING()

	FILE(GLOB_RECURSE UnitTests_SRCS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*Test.cpp" )
	FILE(GLOB_RECURSE UnitTests_INCLUDES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*Test.h" )

	#hackishness
	INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/frontend)
	ADD_EXECUTABLE(UnitTester 
		testsuite/testAll.cpp 
		loader/microX86dis.c
		loader/BinaryFileFactory.cpp 
		loader/BinaryFileStub.cpp 
		${UnitTests_SRCS} 
		${UnitTests_INCLUDES}
	)

	FOREACH(test ${UnitTests_SRCS})
			GET_FILENAME_COMPONENT(TestName ${test} NAME_WE)
			ADD_TEST(${TestName} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/UnitTester "${PROJECT_SOURCE_DIR}" ${TestName})
	ENDFOREACH(test)
	TARGET_LINK_LIBRARIES(UnitTester ${CPPUNIT_LIBRARIES}
		${BOOMERANG_LOADERS} 
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

ENDIF(BUILD_TESTING)
