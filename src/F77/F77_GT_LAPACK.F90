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

MODULE F77_GT_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_GTSV

  SUBROUTINE SGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: NRHS, N, LDB
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
  END SUBROUTINE SGTSV

  SUBROUTINE DGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: NRHS, N, LDB
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
  END SUBROUTINE DGTSV

  SUBROUTINE CGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: NRHS, N, LDB
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
  END SUBROUTINE CGTSV

  SUBROUTINE ZGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: NRHS, N, LDB
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
  END SUBROUTINE ZGTSV

  MODULE PROCEDURE SGTSV1
  MODULE PROCEDURE DGTSV1
  MODULE PROCEDURE CGTSV1
  MODULE PROCEDURE ZGTSV1

END INTERFACE

!---------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_GTSVX

  SUBROUTINE SGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
      & DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
      & WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
    INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(INOUT) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX, *), WORK(*)
    REAL(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
    REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  END SUBROUTINE SGTSVX

  SUBROUTINE DGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
      & DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
      & WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
    INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(INOUT) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX, *), WORK(*)
    REAL(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
    REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  END SUBROUTINE DGTSVX

  SUBROUTINE CGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
      & DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
      & WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
    INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(INOUT) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    COMPLEX(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
    COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  END SUBROUTINE CGTSVX

  SUBROUTINE ZGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
      & DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
      & WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
    INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(INOUT) :: IPIV(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    COMPLEX(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
    COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  END SUBROUTINE ZGTSVX

  MODULE PROCEDURE SGTSVX1
  MODULE PROCEDURE DGTSVX1
  MODULE PROCEDURE CGTSVX1
  MODULE PROCEDURE ZGTSVX1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_GTRFS

  SUBROUTINE SGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
       & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
       & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
    REAL(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*), DLF(*), &
      & DU(*), DU2(*), DUF(*)
    REAL(WP), INTENT(INOUT) :: X(LDX, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SGTRFS

  SUBROUTINE DGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
       & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
       & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
    REAL(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*), DLF(*), &
      & DU(*), DU2(*), DUF(*)
    REAL(WP), INTENT(INOUT) :: X(LDX, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DGTRFS

  SUBROUTINE CGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
       & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
       & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
    COMPLEX(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*),      &
      & DLF(*), DU(*), DU2(*), DUF(*)
    COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CGTRFS

  SUBROUTINE ZGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
       & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
       & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
    COMPLEX(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*),      &
      & DLF(*), DU(*), DU2(*), DUF(*)
    COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZGTRFS

  MODULE PROCEDURE SGTRFS1
  MODULE PROCEDURE DGTRFS1
  MODULE PROCEDURE CGTRFS1
  MODULE PROCEDURE ZGTRFS1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_GTCON

  SUBROUTINE SGTCON(NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
       & WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: NORM
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN) :: IPIV(*)
    REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SGTCON

  SUBROUTINE DGTCON(NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
       & WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: NORM
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN) :: IPIV(*)
    REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DGTCON

  SUBROUTINE CGTCON(NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
       & WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: NORM
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CGTCON

  SUBROUTINE ZGTCON(NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
       & WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: NORM
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZGTCON

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_GTTRS

  SUBROUTINE SGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
       & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    REAL(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE SGTTRS

  SUBROUTINE DGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
       & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    REAL(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE DGTTRS

  SUBROUTINE CGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
       & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE CGTTRS

  SUBROUTINE ZGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
       & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(IN) :: IPIV(*)
    COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
    COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE ZGTTRS

  MODULE PROCEDURE SGTTRS1
  MODULE PROCEDURE DGTTRS1
  MODULE PROCEDURE CGTTRS1
  MODULE PROCEDURE ZGTTRS1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_GTTRF

  SUBROUTINE SGTTRF(N, DL, D, DU, DU2, IPIV, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IPIV(*)
    REAL(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
    REAL(WP), INTENT(OUT) :: DU2(*)
  END SUBROUTINE SGTTRF

  SUBROUTINE DGTTRF(N, DL, D, DU, DU2, IPIV, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IPIV(*)
    REAL(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
    REAL(WP), INTENT(OUT) :: DU2(*)
  END SUBROUTINE DGTTRF

  SUBROUTINE CGTTRF(N, DL, D, DU, DU2, IPIV, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
    COMPLEX(WP), INTENT(OUT) :: DU2(*)
  END SUBROUTINE CGTTRF

  SUBROUTINE ZGTTRF(N, DL, D, DU, DU2, IPIV, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IPIV(*)
    COMPLEX(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
    COMPLEX(WP), INTENT(OUT) :: DU2(*)
  END SUBROUTINE ZGTTRF

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE SGTSV1(N, NRHS, DL, D, DU, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  INTEGER, INTENT(IN) :: NRHS, N, LDB
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
  INTERFACE
    SUBROUTINE SGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      INTEGER, INTENT(IN) :: NRHS, N, LDB
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
    END SUBROUTINE SGTSV
  END INTERFACE
  CALL SGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
END SUBROUTINE SGTSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DGTSV1(N, NRHS, DL, D, DU, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  INTEGER, INTENT(IN) :: NRHS, N, LDB
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
  INTERFACE
    SUBROUTINE DGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      INTEGER, INTENT(IN) :: NRHS, N, LDB
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
    END SUBROUTINE DGTSV
  END INTERFACE
  CALL DGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
END SUBROUTINE DGTSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CGTSV1(N, NRHS, DL, D, DU, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  INTEGER, INTENT(IN) :: NRHS, N, LDB
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
  INTERFACE
    SUBROUTINE CGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      INTEGER, INTENT(IN) :: NRHS, N, LDB
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
    END SUBROUTINE CGTSV
  END INTERFACE
  CALL CGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
END SUBROUTINE CGTSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZGTSV1(N, NRHS, DL, D, DU, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  INTEGER, INTENT(IN) :: NRHS, N, LDB
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
  INTERFACE
    SUBROUTINE ZGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      INTEGER, INTENT(IN) :: NRHS, N, LDB
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB, *)
    END SUBROUTINE ZGTSV
  END INTERFACE
  CALL ZGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
END SUBROUTINE ZGTSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SGTSVX1(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
      & BERR, WORK, IWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
  INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IWORK(*)
  INTEGER, INTENT(INOUT) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, X(*), WORK(*)
  REAL(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*)
  REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  INTERFACE
    SUBROUTINE SGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
      & FERR, BERR, WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
      INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IWORK(*)
      INTEGER, INTENT(INOUT) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX, *), WORK(*)
      REAL(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
      REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
    END SUBROUTINE SGTSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL SGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
      & DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
      & WORK, IWORK, INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE SGTSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DGTSVX1(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
      & BERR, WORK, IWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
  INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IWORK(*)
  INTEGER, INTENT(INOUT) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, X(*), WORK(*)
  REAL(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*)
  REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  INTERFACE
    SUBROUTINE DGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
      & FERR, BERR, WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
      INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IWORK(*)
      INTEGER, INTENT(INOUT) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX, *), WORK(*)
      REAL(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
      REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
    END SUBROUTINE DGTSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL DGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
      & DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
      & WORK, IWORK, INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE DGTSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CGTSVX1(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
      & BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
  INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(INOUT) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
  COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
  COMPLEX(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*)
  COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  INTERFACE
    SUBROUTINE CGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
      & FERR, BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
      INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
      COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      COMPLEX(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
      COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
    END SUBROUTINE CGTSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL CGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
      & DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
      & WORK, RWORK, INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE CGTSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZGTSVX1(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
      & BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
  INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(INOUT) :: IPIV(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
  COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
  COMPLEX(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*)
  COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
  INTERFACE
    SUBROUTINE ZGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
      & DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
      & FERR, BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
      INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: IPIV(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
      COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      COMPLEX(WP), INTENT(IN) :: B(LDB, *), DL(*), D(*), DU(*)
      COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
    END SUBROUTINE ZGTSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL ZGTSVX(FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
      & DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
      & WORK, RWORK, INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE ZGTSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SGTRFS1(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
      & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
      & INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
  INTEGER, INTENT(OUT) :: INFO, IWORK(*)
  REAL(WP), INTENT(OUT) :: BERR, FERR
  REAL(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),      &
      & DU(*), DU2(*), DUF(*)
  REAL(WP), INTENT(INOUT) :: X(*)
  REAL(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE SGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
      & DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
      & WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
      REAL(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*),   &
      & DLF(*), DU(*), DU2(*), DUF(*)
      REAL(WP), INTENT(INOUT) :: X(LDX, *)
      REAL(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE SGTRFS
  END INTERFACE
  REAL(WP) :: FERR1(1), BERR1(1)
  CALL SGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
      & IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
      & INFO)
  FERR = FERR1(1); BERR = BERR1(1)
END SUBROUTINE SGTRFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DGTRFS1(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
      & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
      & INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
  INTEGER, INTENT(OUT) :: INFO, IWORK(*)
  REAL(WP), INTENT(OUT) :: BERR, FERR
  REAL(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),      &
      & DU(*), DU2(*), DUF(*)
  REAL(WP), INTENT(INOUT) :: X(*)
  REAL(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE DGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
      & DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
      & WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
      REAL(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*),   &
      & DLF(*), DU(*), DU2(*), DUF(*)
      REAL(WP), INTENT(INOUT) :: X(LDX, *)
      REAL(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE DGTRFS
  END INTERFACE
  REAL(WP) :: FERR1(1), BERR1(1)
  CALL DGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
      & IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
      & INFO)
  FERR = FERR1(1); BERR = BERR1(1)
END SUBROUTINE DGTRFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CGTRFS1(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
      & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
      & INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
  COMPLEX(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),   &
      & DU(*), DU2(*), DUF(*)
  COMPLEX(WP), INTENT(INOUT) :: X(*)
  COMPLEX(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE CGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
      & DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
      & WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
      COMPLEX(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*),&
      & DLF(*), DU(*), DU2(*), DUF(*)
      COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
      COMPLEX(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE CGTRFS
  END INTERFACE
  REAL(WP) :: FERR1(1), BERR1(1)
  CALL CGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
      & IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, RWORK,  &
      & INFO)
  FERR = FERR1(1); BERR = BERR1(1)
END SUBROUTINE CGTRFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZGTRFS1(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
      & IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
      & INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
  COMPLEX(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),   &
      & DU(*), DU2(*), DUF(*)
  COMPLEX(WP), INTENT(INOUT) :: X(*)
  COMPLEX(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE ZGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
      & DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
      & WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
      COMPLEX(WP), INTENT(IN) :: B(LDB, *), D(*), DF(*), DL(*),&
      & DLF(*), DU(*), DU2(*), DUF(*)
      COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
      COMPLEX(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE ZGTRFS
  END INTERFACE
  REAL(WP) :: FERR1(1), BERR1(1)
  CALL ZGTRFS(TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
      & IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, RWORK,  &
      & INFO)
  FERR = FERR1(1); BERR = BERR1(1)
END SUBROUTINE ZGTRFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SGTTRS1(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
      & INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IPIV(*)
  REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
  REAL(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE SGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
      & LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IPIV(*)
      REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
      REAL(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE SGTTRS
  END INTERFACE
  CALL SGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
      & INFO)
END SUBROUTINE SGTTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DGTTRS1(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
      & INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IPIV(*)
  REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
  REAL(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE DGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
      & LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IPIV(*)
      REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
      REAL(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE DGTTRS
  END INTERFACE
  CALL DGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
      & INFO)
END SUBROUTINE DGTTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CGTTRS1(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
      & INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IPIV(*)
  COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
  COMPLEX(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE CGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
      & LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IPIV(*)
      COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
      COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE CGTTRS
  END INTERFACE
  CALL CGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
      & INFO)
END SUBROUTINE CGTTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZGTTRS1(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
      & INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: TRANS
  INTEGER, INTENT(IN) :: LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IPIV(*)
  COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
  COMPLEX(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE ZGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
      & LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IPIV(*)
      COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
      COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE ZGTTRS
  END INTERFACE
  CALL ZGTTRS(TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
      & INFO)
END SUBROUTINE ZGTTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_GT_LAPACK
