# This program is a part of EASIFEM library
# Copyright (C) 2020-2021  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#
CMAKE_MINIMUM_REQUIRED(VERSION 3.20.0 FATAL_ERROR)
SET(PROJECT_NAME "LAPACK95")
PROJECT(${PROJECT_NAME})
ENABLE_LANGUAGE(Fortran)
SET(VERSION_MAJOR "23")
SET(VERSION_MINOR "8")
SET(VERSION_BugFix "0")
SET(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})
SET(CMAKE_PROJECT_DESCRIPTION "LAPACK95 is a Modern fortran interface to LAPACK")
SET(CMAKE_PROJECT_HOMEPAGE_URL "http://www.netlib.org/lapack95/")
SET(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
SET(namespace "${PROJECT_NAME}")
INCLUDE(CMakePrintHelpers)
LIST( APPEND TARGET_COMPILE_DEF "-DUSE_CMAKE" )
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/checkError.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/makeDirs.cmake)
IF(BUILD_SHARED_LIBS)
  ADD_LIBRARY(${PROJECT_NAME} SHARED "" )
  SET_PROPERTY(TARGET ${PROJECT_NAME} PROPERTY POSITION_INDEPENDENT_CODE TRUE)
ELSE()
  ADD_LIBRARY(${PROJECT_NAME} STATIC "")
ENDIF()
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/Compiler.cmake)
# Add source files
INCLUDE(${CMAKE_SOURCE_DIR}/src/CMakeLists.txt)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addOpenBLAS.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addOpenMP.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetLinkLibs.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetIncludeDirs.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetCompileOpts.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetCompileDefs.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetProperties.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/install.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/packaging.cmake)

