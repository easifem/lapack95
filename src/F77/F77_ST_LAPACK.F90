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

MODULE F77_ST_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEGR

  SUBROUTINE SSTEGR(JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
&                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
&                      IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M
    INTEGER, INTENT(OUT) :: ISUPPZ(*), IWORK(LIWORK)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(IN) :: W(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE SSTEGR

  SUBROUTINE DSTEGR(JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
&                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
&                      IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M
    INTEGER, INTENT(OUT) :: ISUPPZ(*), IWORK(LIWORK)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(IN) :: W(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE DSTEGR

  SUBROUTINE CSTEGR(JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
&                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
&                      IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M
    INTEGER, INTENT(OUT) :: ISUPPZ(*), IWORK(LIWORK)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(IN) :: W(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE CSTEGR

  SUBROUTINE ZSTEGR(JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
&                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
&                      IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M
    INTEGER, INTENT(OUT) :: ISUPPZ(*), IWORK(LIWORK)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(IN) :: W(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE ZSTEGR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEIN

  SUBROUTINE SSTEIN(N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
 &                   IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
    INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
    REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE SSTEIN

  SUBROUTINE DSTEIN(N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
 &                   IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
    INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
    REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE DSTEIN

  SUBROUTINE CSTEIN(N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
 &                   IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
    INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
    REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE CSTEIN

  SUBROUTINE ZSTEIN(N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
 &                   IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
    INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
    REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *)
  END SUBROUTINE ZSTEIN

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEBZ

  SUBROUTINE SSTEBZ(RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
 &                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,     &
 &                   INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: ORDER, RANGE
    INTEGER, INTENT(IN) :: IL, IU, M, N
    INTEGER, INTENT(OUT) :: INFO, NSPLIT, IBLOCK(*), ISPLIT(*),    &
&                           IWORK(*)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU, D(*), E(*)
    REAL(WP), INTENT(OUT) :: W(*), WORK(*)
  END SUBROUTINE SSTEBZ

  SUBROUTINE DSTEBZ(RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
 &                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,     &
 &                   INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: ORDER, RANGE
    INTEGER, INTENT(IN) :: IL, IU, M, N
    INTEGER, INTENT(OUT) :: INFO, NSPLIT, IBLOCK(*), ISPLIT(*),    &
&                           IWORK(*)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU, D(*), E(*)
    REAL(WP), INTENT(OUT) :: W(*), WORK(*)
  END SUBROUTINE DSTEBZ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEDC

  SUBROUTINE SSTEDC(COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
 &                   LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(INOUT) :: Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SSTEDC

  SUBROUTINE DSTEDC(COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
 &                   LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(INOUT) :: Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DSTEDC

  SUBROUTINE CSTEDC(COMPZ, N, D, E, Z, LDZ, WORK, LWORK, RWORK,    &
 &                   LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, LIWORK, LRWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: RWORK(LRWORK)
    COMPLEX(WP), INTENT(INOUT) :: Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CSTEDC

  SUBROUTINE ZSTEDC(COMPZ, N, D, E, Z, LDZ, WORK, LWORK, RWORK,    &
 &                   LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, LIWORK, LRWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: RWORK(LRWORK)
    COMPLEX(WP), INTENT(INOUT) :: Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZSTEDC

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STERF

  SUBROUTINE SSTERF(N, D, E, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
  END SUBROUTINE SSTERF

  SUBROUTINE DSTERF(N, D, E, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
  END SUBROUTINE DSTERF

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEQR

  SUBROUTINE SSTEQR(COMPZ, N, D, E, Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    REAL(WP), INTENT(INOUT) :: Z(LDZ, *)
  END SUBROUTINE SSTEQR

  SUBROUTINE DSTEQR(COMPZ, N, D, E, Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    REAL(WP), INTENT(INOUT) :: Z(LDZ, *)
  END SUBROUTINE DSTEQR

  SUBROUTINE CSTEQR(COMPZ, N, D, E, Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    COMPLEX(WP), INTENT(INOUT) :: Z(LDZ, *)
  END SUBROUTINE CSTEQR

  SUBROUTINE ZSTEQR(COMPZ, N, D, E, Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
    COMPLEX(WP), INTENT(INOUT) :: Z(LDZ, *)
  END SUBROUTINE ZSTEQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEVR

  SUBROUTINE SSTEVR(JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
&                    M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,     &
&                    LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LWORK, LIWORK
    INTEGER, INTENT(OUT) :: M
    INTEGER, INTENT(OUT), TARGET :: ISUPPZ(*)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: WORK(*), W(*)
    REAL(WP), INTENT(OUT), TARGET :: Z(LDZ, *)
  END SUBROUTINE SSTEVR

  SUBROUTINE DSTEVR(JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
&                    M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,     &
&                    LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LWORK, LIWORK
    INTEGER, INTENT(OUT) :: M
    INTEGER, INTENT(OUT), TARGET :: ISUPPZ(*)
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: WORK(*), W(*)
    REAL(WP), INTENT(OUT), TARGET :: Z(LDZ, *)
  END SUBROUTINE DSTEVR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEVX

  SUBROUTINE SSTEVX(JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
&                    M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: W(*), Z(LDZ, *), WORK(*)
  END SUBROUTINE SSTEVX

  SUBROUTINE DSTEVX(JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
&                    M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: W(*), Z(LDZ, *), WORK(*)
  END SUBROUTINE DSTEVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEVD

  SUBROUTINE SSTEVD(JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
&                    LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ
    INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSTEVD

  SUBROUTINE DSTEVD(JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
&                    LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ
    INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSTEVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_STEV

  SUBROUTINE SSTEV(JOBZ, N, D, E, Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSTEV

  SUBROUTINE DSTEV(JOBZ, N, D, E, Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: D(*), E(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSTEV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_ST_LAPACK
