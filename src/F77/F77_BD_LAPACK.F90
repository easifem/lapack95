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

MODULE F77_BD_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_BDSDC

  SUBROUTINE SBDSDC(UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q,  &
&                      IQ, WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPQ, UPLO
    INTEGER, INTENT(IN) :: LDU, LDVT, N
    INTEGER, INTENT(OUT) :: INFO, IQ(*), IWORK(*)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: Q(*), U(LDU, *), VT(LDVT, *), WORK(*)
  END SUBROUTINE SBDSDC

  SUBROUTINE DBDSDC(UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q,  &
&                      IQ, WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPQ, UPLO
    INTEGER, INTENT(IN) :: LDU, LDVT, N
    INTEGER, INTENT(OUT) :: INFO, IQ(*), IWORK(*)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: Q(*), U(LDU, *), VT(LDVT, *), WORK(*)
  END SUBROUTINE DBDSDC

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_BDSQR

  SUBROUTINE SBDSQR(UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
 &                   LDU, C, LDC, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
    INTEGER, INTENT(OUT) :: INFO
    REAL, INTENT(INOUT) :: D(*), E(*)
    REAL, INTENT(OUT) :: RWORK(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *), U(LDU, *), VT(LDVT, *)
  END SUBROUTINE SBDSQR

  SUBROUTINE DBDSQR(UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
 &                   LDU, C, LDC, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
    INTEGER, INTENT(OUT) :: INFO
    REAL, INTENT(INOUT) :: D(*), E(*)
    REAL, INTENT(OUT) :: RWORK(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *), U(LDU, *), VT(LDVT, *)
  END SUBROUTINE DBDSQR

  SUBROUTINE CBDSQR(UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
 &                   LDU, C, LDC, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
    INTEGER, INTENT(OUT) :: INFO
    REAL, INTENT(INOUT) :: D(*), E(*)
    REAL, INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *), U(LDU, *), VT(LDVT, *)
  END SUBROUTINE CBDSQR

  SUBROUTINE ZBDSQR(UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
 &                   LDU, C, LDC, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
    INTEGER, INTENT(OUT) :: INFO
    REAL, INTENT(INOUT) :: D(*), E(*)
    REAL, INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *), U(LDU, *), VT(LDVT, *)
  END SUBROUTINE ZBDSQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_BD_LAPACK
