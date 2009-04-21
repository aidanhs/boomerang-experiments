SET(boomerang_db_SOURCES
	basicblock.cpp
	cfg.cpp
	dataflow.cpp
	exp.cpp
	insnameelem.cpp
	managed.cpp
	project.cpp
	proc.cpp
	prog.cpp
	register.cpp
	rtl.cpp
	signature.cpp
	sslinst.cpp
	sslparser.cpp
	sslscanner.cpp
	statement.cpp
	table.cpp
	visitor.cpp
	section_info.cpp
	${MAIN_INCLUDE_DIRECTORY}/db/section_info.h
	${MAIN_INCLUDE_DIRECTORY}/db/project.h
)
# for ansi-c parser includes
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/c)

ADD_LIBRARY(boomerang_db STATIC ${boomerang_db_SOURCES})
