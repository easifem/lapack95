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

MODULE F77_OR_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMRZ

  SUBROUTINE SORMRZ(SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, &
    & LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMRZ

  SUBROUTINE DORMRZ(SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, &
    & LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMRZ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMBR

  SUBROUTINE SORMBR(VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C, &
      & LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMBR

  SUBROUTINE DORMBR(VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C, &
      & LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMBR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGBR

  SUBROUTINE SORGBR(VECT, M, N, K, A, LDA, TAU, WORK, LWORK, &
      & INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: VECT
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGBR

  SUBROUTINE DORGBR(VECT, M, N, K, A, LDA, TAU, WORK, LWORK, &
      & INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: VECT
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGBR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMHR

  SUBROUTINE SORMHR(SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, &
      & LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMHR

  SUBROUTINE DORMHR(SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, &
      & LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMHR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGHR

  SUBROUTINE SORGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGHR

  SUBROUTINE DORGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGHR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMTR

  SUBROUTINE SORMTR(SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
    INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
  END SUBROUTINE SORMTR

  SUBROUTINE DORMTR(SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
    INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
  END SUBROUTINE DORMTR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMRQ

  SUBROUTINE SORMRQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMRQ

  SUBROUTINE DORMRQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMRQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGRQ

  SUBROUTINE SORGRQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGRQ

  SUBROUTINE DORGRQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGRQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMQL

  SUBROUTINE SORMQL(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMQL

  SUBROUTINE DORMQL(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMQL

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGQL

  SUBROUTINE SORGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGQL

  SUBROUTINE DORGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGQL

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMLQ

  SUBROUTINE SORMLQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMLQ

  SUBROUTINE DORMLQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMLQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGLQ

  SUBROUTINE SORGLQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGLQ

  SUBROUTINE DORGLQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGLQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORMQR

  SUBROUTINE SORMQR(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORMQR

  SUBROUTINE DORMQR(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
      & WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    REAL(WP), INTENT(INOUT) :: C(LDC, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORMQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGQR

  SUBROUTINE SORGQR(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGQR

  SUBROUTINE DORGQR(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_ORGTR

  SUBROUTINE SORGTR(UPLO, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SORGTR

  SUBROUTINE DORGTR(UPLO, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TAU(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DORGTR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_OR_LAPACK
