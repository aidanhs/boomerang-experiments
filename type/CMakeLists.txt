INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/platform/include/db)

SET(boomerang_typesystem_sources
type.cpp
dfa.cpp
constraint.cpp
)

ADD_LIBRARY(boomerang_type_solvers STATIC ${boomerang_typesystem_sources})
