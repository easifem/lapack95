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

MODULE F77_PO_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POEQU

  SUBROUTINE SPOEQU(N, A, LDA, S, SCOND, AMAX, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
    REAL(WP), INTENT(IN) :: A(LDA, *)
  END SUBROUTINE SPOEQU

  SUBROUTINE DPOEQU(N, A, LDA, S, SCOND, AMAX, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
    REAL(WP), INTENT(IN) :: A(LDA, *)
  END SUBROUTINE DPOEQU

  SUBROUTINE CPOEQU(N, A, LDA, S, SCOND, AMAX, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *)
  END SUBROUTINE CPOEQU

  SUBROUTINE ZPOEQU(N, A, LDA, S, SCOND, AMAX, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *)
  END SUBROUTINE ZPOEQU

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POTRI

  SUBROUTINE SPOTRI(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE SPOTRI

  SUBROUTINE DPOTRI(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE DPOTRI

  SUBROUTINE CPOTRI(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE CPOTRI

  SUBROUTINE ZPOTRI(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE ZPOTRI

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_PORFS

  SUBROUTINE SPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
 &                   LDX, FERR, BERR, WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
    REAL(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    REAL(WP), INTENT(INOUT) :: X(LDX, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SPORFS

  SUBROUTINE DPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
 &                   LDX, FERR, BERR, WORK, IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
    REAL(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    REAL(WP), INTENT(INOUT) :: X(LDX, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DPORFS

  SUBROUTINE CPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
 &                   LDX, FERR, BERR, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CPORFS

  SUBROUTINE ZPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
 &                   LDX, FERR, BERR, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZPORFS

  MODULE PROCEDURE SPORFS1
  MODULE PROCEDURE DPORFS1
  MODULE PROCEDURE CPORFS1
  MODULE PROCEDURE ZPORFS1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POTRS

  SUBROUTINE SPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *)
    REAL(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE SPOTRS

  SUBROUTINE DPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *)
    REAL(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE DPOTRS

  SUBROUTINE CPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *)
    COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE CPOTRS

  SUBROUTINE ZPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *)
    COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
  END SUBROUTINE ZPOTRS

  MODULE PROCEDURE SPOTRS1
  MODULE PROCEDURE DPOTRS1
  MODULE PROCEDURE CPOTRS1
  MODULE PROCEDURE ZPOTRS1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POSV

  SUBROUTINE SPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
  END SUBROUTINE SPOSV

  SUBROUTINE DPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
  END SUBROUTINE DPOSV

  SUBROUTINE CPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
  END SUBROUTINE CPOSV

  SUBROUTINE ZPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
  END SUBROUTINE ZPOSV

  MODULE PROCEDURE SPOSV1
  MODULE PROCEDURE DPOSV1
  MODULE PROCEDURE CPOSV1
  MODULE PROCEDURE ZPOSV1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POSVX

  SUBROUTINE SPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
&                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
&                    IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
    CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IWORK(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
    REAL(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    REAL(WP), INTENT(INOUT) :: S(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
  END SUBROUTINE SPOSVX

  SUBROUTINE DPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
&                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
&                    IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
    CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    INTEGER, INTENT(OUT) :: IWORK(*)
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
    REAL(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    REAL(WP), INTENT(INOUT) :: S(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
  END SUBROUTINE DPOSVX

  SUBROUTINE CPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
&                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
&                    RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
    CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    REAL(WP), INTENT(INOUT) :: S(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
  END SUBROUTINE CPOSVX

  SUBROUTINE ZPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
&                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
&                    RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
    CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
    INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: RCOND
    REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
    REAL(WP), INTENT(INOUT) :: S(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
  END SUBROUTINE ZPOSVX

  MODULE PROCEDURE SPOSVX1
  MODULE PROCEDURE DPOSVX1
  MODULE PROCEDURE CPOSVX1
  MODULE PROCEDURE ZPOSVX1

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POTRF

  SUBROUTINE SPOTRF(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE SPOTRF

  SUBROUTINE DPOTRF(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE DPOTRF

  SUBROUTINE CPOTRF(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE CPOTRF

  SUBROUTINE ZPOTRF(UPLO, N, A, LDA, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
  END SUBROUTINE ZPOTRF

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_POCON

  SUBROUTINE SPOCON(UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
&                    INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(OUT) :: IWORK(*)
    REAL(WP), INTENT(IN) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SPOCON

  SUBROUTINE DPOCON(UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
&                    INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND
    INTEGER, INTENT(OUT) :: IWORK(*)
    REAL(WP), INTENT(IN) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DPOCON

  SUBROUTINE CPOCON(UPLO, N, A, LDA, ANORM, RCOND, WORK, RWORK,   &
&                    INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CPOCON

  SUBROUTINE ZPOCON(UPLO, N, A, LDA, ANORM, RCOND, WORK, RWORK,   &
&                    INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: ANORM
    REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZPOCON

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SPOTRS1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(IN) :: A(LDA, *)
  REAL(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE SPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(IN) :: A(LDA, *)
      REAL(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE SPOTRS
  END INTERFACE
  CALL SPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE SPOTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DPOTRS1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(IN) :: A(LDA, *)
  REAL(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE DPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(IN) :: A(LDA, *)
      REAL(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE DPOTRS
  END INTERFACE
  CALL DPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE DPOTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CPOTRS1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(WP), INTENT(IN) :: A(LDA, *)
  COMPLEX(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE CPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(IN) :: A(LDA, *)
      COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE CPOTRS
  END INTERFACE
  CALL CPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE CPOTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZPOTRS1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(WP), INTENT(IN) :: A(LDA, *)
  COMPLEX(WP), INTENT(INOUT) :: B(*)
  INTERFACE
    SUBROUTINE ZPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(IN) :: A(LDA, *)
      COMPLEX(WP), INTENT(INOUT) :: B(LDB, *)
    END SUBROUTINE ZPOTRS
  END INTERFACE
  CALL ZPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE ZPOTRS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SPORFS1(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
&                    LDX, FERR, BERR, WORK, IWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
  INTEGER, INTENT(OUT) :: INFO, IWORK(*)
  REAL(WP), INTENT(OUT) :: BERR, FERR
  REAL(WP), INTENT(IN) :: A(*), AF(LDAF, *), B(LDB, *)
  REAL(WP), INTENT(INOUT) :: X(*)
  REAL(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE SPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
&                         X, LDX, FERR, BERR, WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
      REAL(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *),          &
&                                 B(LDB, *)
      REAL(WP), INTENT(INOUT) :: X(LDX, *)
      REAL(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE SPORFS
  END INTERFACE
  REAL(WP) :: BERR1(1), FERR1(1)
  CALL SPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
&                FERR1, BERR1, WORK, IWORK, INFO)
  BERR = BERR1(1); FERR = FERR1(1)
END SUBROUTINE SPORFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DPORFS1(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
&                    LDX, FERR, BERR, WORK, IWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
  INTEGER, INTENT(OUT) :: INFO, IWORK(*)
  REAL(WP), INTENT(OUT) :: BERR, FERR
  REAL(WP), INTENT(IN) :: A(*), AF(LDAF, *), B(LDB, *)
  REAL(WP), INTENT(INOUT) :: X(*)
  REAL(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE DPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
&                         X, LDX, FERR, BERR, WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
      REAL(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *),          &
&                                 B(LDB, *)
      REAL(WP), INTENT(INOUT) :: X(LDX, *)
      REAL(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE DPORFS
  END INTERFACE
  REAL(WP) :: BERR1(1), FERR1(1)
  CALL DPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
&                FERR1, BERR1, WORK, IWORK, INFO)
  BERR = BERR1(1); FERR = FERR1(1)
END SUBROUTINE DPORFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CPORFS1(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
&                    LDX, FERR, BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
  COMPLEX(WP), INTENT(IN) :: A(*), AF(LDAF, *), B(LDB, *)
  COMPLEX(WP), INTENT(INOUT) :: X(*)
  COMPLEX(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE CPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
&                         X, LDX, FERR, BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
      COMPLEX(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *),       &
&                                    B(LDB, *)
      COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
      COMPLEX(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE CPORFS
  END INTERFACE
  REAL(WP) :: BERR1(1), FERR1(1)
  CALL CPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
&                FERR1, BERR1, WORK, RWORK, INFO)
  BERR = BERR1(1); FERR = FERR1(1)
END SUBROUTINE CPORFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZPORFS1(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
&                    LDX, FERR, BERR, WORK, RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
  COMPLEX(WP), INTENT(IN) :: A(*), AF(LDAF, *), B(LDB, *)
  COMPLEX(WP), INTENT(INOUT) :: X(*)
  COMPLEX(WP), INTENT(OUT) :: WORK(*)
  INTERFACE
    SUBROUTINE ZPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
&                         X, LDX, FERR, BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
      COMPLEX(WP), INTENT(IN) :: A(LDA, *), AF(LDAF, *),       &
&                                    B(LDB, *)
      COMPLEX(WP), INTENT(INOUT) :: X(LDX, *)
      COMPLEX(WP), INTENT(OUT) :: WORK(*)
    END SUBROUTINE ZPORFS
  END INTERFACE
  REAL(WP) :: BERR1(1), FERR1(1)
  CALL ZPORFS(UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
&                FERR1, BERR1, WORK, RWORK, INFO)
  BERR = BERR1(1); FERR = FERR1(1)
END SUBROUTINE ZPORFS1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SPOSV1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(INOUT) :: A(LDA, *), B(*)
  INTERFACE
    SUBROUTINE SPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
    END SUBROUTINE SPOSV
  END INTERFACE
  CALL SPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE SPOSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DPOSV1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(INOUT) :: A(LDA, *), B(*)
  INTERFACE
    SUBROUTINE DPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
    END SUBROUTINE DPOSV
  END INTERFACE
  CALL DPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE DPOSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CPOSV1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(*)
  INTERFACE
    SUBROUTINE CPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
    END SUBROUTINE CPOSV
  END INTERFACE
  CALL CPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE CPOSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZPOSV1(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: UPLO
  INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(*)
  INTERFACE
    SUBROUTINE ZPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *)
    END SUBROUTINE ZPOSV
  END INTERFACE
  CALL ZPOSV(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
END SUBROUTINE ZPOSV1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SPOSVX1(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
&                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
&                     IWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
  CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IWORK(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR
  REAL(WP), INTENT(OUT) :: X(*), WORK(*)
  REAL(WP), INTENT(INOUT) :: S(*)
  REAL(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(*)
  INTERFACE
    SUBROUTINE SPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
&                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
&                        BERR, WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IWORK(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
      REAL(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      REAL(WP), INTENT(INOUT) :: S(*)
      REAL(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    END SUBROUTINE SPOSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL SPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
&                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, IWORK, &
&                INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE SPOSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE DPOSVX1(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
&                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
&                     IWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
  CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  INTEGER, INTENT(OUT) :: IWORK(*)
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR
  REAL(WP), INTENT(OUT) :: X(*), WORK(*)
  REAL(WP), INTENT(INOUT) :: S(*)
  REAL(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(*)
  INTERFACE
    SUBROUTINE DPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
&                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
&                        BERR, WORK, IWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(OUT) :: IWORK(*)
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
      REAL(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      REAL(WP), INTENT(INOUT) :: S(*)
      REAL(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    END SUBROUTINE DPOSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL DPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
&                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, IWORK, &
&                INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE DPOSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CPOSVX1(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
&                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
&                     RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
  CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
  COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
  REAL(WP), INTENT(INOUT) :: S(*)
  COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(*)
  INTERFACE
    SUBROUTINE CPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
&                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
&                        BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => SP
      CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
      COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      REAL(WP), INTENT(INOUT) :: S(*)
      COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    END SUBROUTINE CPOSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL CPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
&                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, RWORK, &
&                INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE CPOSVX1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ZPOSVX1(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
&                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
&                     RWORK, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
  CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
  INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
  INTEGER, INTENT(OUT) :: INFO
  REAL(WP), INTENT(OUT) :: RCOND
  REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
  COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
  REAL(WP), INTENT(INOUT) :: S(*)
  COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(*)
  INTERFACE
    SUBROUTINE ZPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
&                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
&                        BERR, WORK, RWORK, INFO)
      USE LA_PRECISION, ONLY: WP => DP
      CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: RCOND
      REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
      COMPLEX(WP), INTENT(OUT) :: X(LDX, *), WORK(*)
      REAL(WP), INTENT(INOUT) :: S(*)
      COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), AF(LDAF, *), B(LDB, *)
    END SUBROUTINE ZPOSVX
  END INTERFACE
  REAL(WP) :: LFERR(1), LBERR(1)
  CALL ZPOSVX(FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
&                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, RWORK, &
&                INFO)
  FERR = LFERR(1); BERR = LBERR(1)
END SUBROUTINE ZPOSVX1

END MODULE F77_PO_LAPACK
