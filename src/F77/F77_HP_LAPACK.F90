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

MODULE F77_HP_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPGST

  SUBROUTINE CHPGST(ITYPE, UPLO, N, AP, BP, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: ITYPE, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: BP(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
  END SUBROUTINE CHPGST

  SUBROUTINE ZHPGST(ITYPE, UPLO, N, AP, BP, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: ITYPE, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: BP(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
  END SUBROUTINE ZHPGST

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPTRD

  SUBROUTINE CHPTRD(UPLO, N, AP, D, E, TAU, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: D(*), E(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    COMPLEX(WP), INTENT(OUT) :: TAU(*)
  END SUBROUTINE CHPTRD

  SUBROUTINE ZHPTRD(UPLO, N, AP, D, E, TAU, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: D(*), E(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    COMPLEX(WP), INTENT(OUT) :: TAU(*)
  END SUBROUTINE ZHPTRD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPTRI

  SUBROUTINE CHPTRI(UPLO, N, AP, IPIV, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CHPTRI

  SUBROUTINE ZHPTRI(UPLO, N, AP, IPIV, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZHPTRI

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPRFS

  SUBROUTINE CHPRFS(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
   & FERR, BERR, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
    COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CHPRFS

  SUBROUTINE ZHPRFS(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
   & FERR, BERR, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
    COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZHPRFS

  MODULE PROCEDURE CHPRFS1
  MODULE PROCEDURE ZHPRFS1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPCON

  SUBROUTINE CHPCON(UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: AP(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CHPCON

  SUBROUTINE ZHPCON(UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: AP(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZHPCON

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPTRS

  SUBROUTINE CHPTRS(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: AP(*)
    COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE CHPTRS

  SUBROUTINE ZHPTRS(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: AP(*)
    COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE ZHPTRS

  MODULE PROCEDURE CHPTRS1
  MODULE PROCEDURE ZHPTRS1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPTRF

  SUBROUTINE CHPTRF(UPLO, N, AP, IPIV, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO, IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
  END SUBROUTINE CHPTRF

  SUBROUTINE ZHPTRF(UPLO, N, AP, IPIV, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO, IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
  END SUBROUTINE ZHPTRF

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPGVX

  SUBROUTINE CHPGVX(ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU, &
  & IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, &
  & IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE CHPGVX

  SUBROUTINE ZHPGVX(ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU, &
  & IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, &
  & IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE ZHPGVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPGVD

  SUBROUTINE CHPGVD(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,&
  & LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHPGVD

  SUBROUTINE ZHPGVD(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,&
  & LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHPGVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPGV

  SUBROUTINE CHPGV(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
  & RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHPGV

  SUBROUTINE ZHPGV(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
  & RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHPGV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPEVX

  SUBROUTINE CHPEVX(JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU, &
  & ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, &
  & IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHPEVX

  SUBROUTINE ZHPEVX(JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU, &
  & ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, &
  & IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHPEVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPEVD

  SUBROUTINE CHPEVD(JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK, &
  & RWORK, LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK, LRWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHPEVD

  SUBROUTINE ZHPEVD(JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK, &
  & RWORK, LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK, LRWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHPEVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPEV

  SUBROUTINE CHPEV(JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK, &
  & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHPEV

  SUBROUTINE ZHPEV(JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK, &
  & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: LDZ, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AP(*)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHPEV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPSV

  SUBROUTINE CHPSV(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: NRHS, N, LDB
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB, *)
  END SUBROUTINE CHPSV

  SUBROUTINE ZHPSV(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: NRHS, N, LDB
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB, *)
  END SUBROUTINE ZHPSV

  MODULE PROCEDURE CHPSV1
  MODULE PROCEDURE ZHPSV1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HPSVX

  SUBROUTINE CHPSVX(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
  & LDX, RCOND, FERR, BERR, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
    INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(INOUT) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    COMPLEX(WP), INTENT(IN) :: A(*), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: AF(*)
  END SUBROUTINE CHPSVX

  SUBROUTINE ZHPSVX(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
  & LDX, RCOND, FERR, BERR, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
    INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(INOUT) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    COMPLEX(WP), INTENT(IN) :: A(*), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: AF(*)
  END SUBROUTINE ZHPSVX

  MODULE PROCEDURE CHPSVX1
  MODULE PROCEDURE ZHPSVX1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE CHPRFS1(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
  & FERR, BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(IN) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
  COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB, *)
  COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
  COMPLEX(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE CHPRFS(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
  & LDX, FERR, BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(IN) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
      COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB, *)
      COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
      COMPLEX(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE CHPRFS
  END INTERFACE
  REAL(WP) :: FERR1(1), BERR1(1)
  CALL CHPRFS(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
  & FERR1, BERR1, WORK, RWORK, INFO)
  FERR = FERR1(1); BERR = BERR1(1)
END SUBROUTINE CHPRFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZHPRFS1(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
  & FERR, BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(IN) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
  COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB, *)
  COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
  COMPLEX(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE ZHPRFS(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
  & LDX, FERR, BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(IN) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
      COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB, *)
      COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
      COMPLEX(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE ZHPRFS
  END INTERFACE
  REAL(WP) :: FERR1(1), BERR1(1)
  CALL ZHPRFS(UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
  & FERR1, BERR1, WORK, RWORK, INFO)
  FERR = FERR1(1); BERR = BERR1(1)
END SUBROUTINE ZHPRFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CHPTRS1(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(IN) :: IPIV(*)
  COMPLEX(WP), INTENT(IN) :: AP(*)
  COMPLEX(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE CHPTRS(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(IN) :: IPIV(*)
      COMPLEX(WP), INTENT(IN) :: AP(*)
      COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE CHPTRS
  END INTERFACE
  CALL CHPTRS(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
END SUBROUTINE CHPTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZHPTRS1(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(IN) :: IPIV(*)
  COMPLEX(WP), INTENT(IN) :: AP(*)
  COMPLEX(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE ZHPTRS(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(IN) :: IPIV(*)
      COMPLEX(WP), INTENT(IN) :: AP(*)
      COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE ZHPTRS
  END INTERFACE
  CALL ZHPTRS(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
END SUBROUTINE ZHPTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CHPSV1(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: NRHS, N, LDB
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(IN) :: IPIV(*)
  COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
  INTERFACE
    SUBROUTINE CHPSV(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: NRHS, N, LDB
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(IN) :: IPIV(*)
      COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB, *)
    END SUBROUTINE CHPSV
  END INTERFACE
  CALL CHPSV(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
END SUBROUTINE CHPSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZHPSV1(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: NRHS, N, LDB
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(IN) :: IPIV(*)
  COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
  INTERFACE
    SUBROUTINE ZHPSV(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: NRHS, N, LDB
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(IN) :: IPIV(*)
      COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB, *)
    END SUBROUTINE ZHPSV
  END INTERFACE
  CALL ZHPSV(UPLO, N, NRHS, AP, IPIV, B, LDB, INFO)
END SUBROUTINE ZHPSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CHPSVX1(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
  & LDX, RCOND, FERR, BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
  INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(INOUT) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
  COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
  COMPLEX(WP), INTENT(IN) :: A(*), B(*)
  COMPLEX(WP), INTENT(INOUT) :: AF(*)
  INTERFACE
    SUBROUTINE CHPSVX(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
  & X, LDX, RCOND, FERR, BERR, WORK, RWORK, &
  & INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
      INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
      COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      COMPLEX(WP), INTENT(IN) :: A(*), B(LDB, *)
      COMPLEX(WP), INTENT(INOUT) :: AF(*)
    END SUBROUTINE CHPSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL CHPSVX(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
  & RCOND, LFERR, LBERR, WORK, RWORK, INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE CHPSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZHPSVX1(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
  & LDX, RCOND, FERR, BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
  INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(INOUT) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
  COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
  COMPLEX(WP), INTENT(IN) :: A(*), B(*)
  COMPLEX(WP), INTENT(INOUT) :: AF(*)
  INTERFACE
    SUBROUTINE ZHPSVX(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
  & X, LDX, RCOND, FERR, BERR, WORK, RWORK, &
  & INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
      INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
      COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      COMPLEX(WP), INTENT(IN) :: A(*), B(LDB, *)
      COMPLEX(WP), INTENT(INOUT) :: AF(*)
    END SUBROUTINE ZHPSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL ZHPSVX(FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
  & RCOND, LFERR, LBERR, WORK, RWORK, INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE ZHPSVX1

END MODULE F77_HP_LAPACK
