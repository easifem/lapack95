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


OPTION(USE_OpenMP OFF)

MESSAGE(STATUS "CMAKE_HOST_SYSTEM_PROCESSOR: ${CMAKE_HOST_SYSTEM_PROCESSOR}")
MESSAGE(STATUS "CMAKE_SYSTEM_PROCESSOR: ${CMAKE_SYSTEM_PROCESSOR}")

IF(USE_OpenMP)
  IF(APPLE)
    IF(CMAKE_C_COMPILER_ID MATCHES "Clang" OR CMAKE_C_COMPILER_ID MATCHES "AppleClang")

      SET(OpenMP_C "${CMAKE_C_COMPILER}" CACHE STRING "" FORCE)
      SET(OpenMP_C_FLAGS 
          "-fopenmp=libomp -Wno-unused-command-line-argument" 
          CACHE STRING 
          "" 
          FORCE
      )

      SET(OpenMP_C_LIB_NAMES "libomp" "libgomp" "libiomp5" CACHE STRING "" FORCE)
      SET(OpenMP_libomp_LIBRARY ${OpenMP_C_LIB_NAMES} CACHE STRING "" FORCE)
      SET(OpenMP_libgomp_LIBRARY ${OpenMP_C_LIB_NAMES} CACHE STRING "" FORCE)
      SET(OpenMP_libiomp5_LIBRARY ${OpenMP_C_LIB_NAMES} CACHE STRING "" FORCE)

      SET(OpenMP_CXX "${CMAKE_CXX_COMPILER}" CACHE STRING "" FORCE)
      SET(
        OpenMP_CXX_FLAGS 
        "-fopenmp=libomp -Wno-unused-command-line-argument" 
        CACHE STRING 
        "" 
        FORCE
      )

      SET(OpenMP_CXX_LIB_NAMES "libomp" "libgomp" "libiomp5" CACHE STRING "" FORCE)
    ENDIF()
  ENDIF()

  FIND_PACKAGE(OpenMP REQUIRED)

ENDIF()


IF(OpenMP_FOUND)
  MESSAGE(STATUS "FOUND OpenMP")
  MESSAGE(STATUS "OpenMP_Fortran_LIBRARIES: ${OpenMP_Fortran_LIBRARIES}")
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_OpenMP" )
  LIST( APPEND TARGET_COMPILE_OPT ${OpenMP_Fortran_FLAGS} )
  # TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${OpenMP_Fortran_LIBRARIES})
  TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC OpenMP::OpenMP_Fortran)
  ELSE()
  MESSAGE(ERROR "NOT FOUND OpenMP!!")
ENDIF()

