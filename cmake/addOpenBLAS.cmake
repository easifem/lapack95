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

#SET(BLA_VENDOR "OpenBLAS")
FIND_PACKAGE( LAPACK REQUIRED )
IF( LAPACK_FOUND )
  MESSAGE(STATUS "FOUND LAPACK")
  MESSAGE("LAPACK_LIBRARIES: ${LAPACK_LIBRARIES}")
  MESSAGE("BLAS_LIBRARIES: ${BLAS_LIBRARIES}")
  MESSAGE("BLA_vendor: ${BLA_VENDOR}")
ENDIF()

IF( BLA_VENDOR MATCHES "MKL" )
  MESSAGE(STATUS "BLA_VENDOR : MKL")
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_INTEL_MKL" )
ELSEIF( BLA_VENDOR MATCHES "OpenBLAS" )
  MESSAGE(STATUS "BLA_VENDOR : OpenBLAS")
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_OpenBLAS" )
ELSE( )
  MESSAGE(STATUS "BLA_VENDOR : System provided")
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_NativeBLAS" )
  IF(APPLE)
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_APPLE_NativeBLAS" )
  ENDIF()
ENDIF( )
