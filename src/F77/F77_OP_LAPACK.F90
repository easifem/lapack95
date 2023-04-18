! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE F77_OP_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_OPMTR

  SUBROUTINE SOPMTR(SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
      & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
    INTEGER, INTENT(IN) :: LDC, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: AP(*), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SOPMTR

  SUBROUTINE DOPMTR(SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
      & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
    INTEGER, INTENT(IN) :: LDC, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: AP(*), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DOPMTR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_OPGTR

  SUBROUTINE SOPGTR(UPLO, N, AP, TAU, Q, LDQ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDQ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: AP(*), TAU(*)
    REAL(WP), INTENT(OUT) :: Q(LDQ, *), WORK(*)
  END SUBROUTINE SOPGTR

  SUBROUTINE DOPGTR(UPLO, N, AP, TAU, Q, LDQ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDQ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: AP(*), TAU(*)
    REAL(WP), INTENT(OUT) :: Q(LDQ, *), WORK(*)
  END SUBROUTINE DOPGTR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_OP_LAPACK
