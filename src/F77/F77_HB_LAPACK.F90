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

MODULE F77_HB_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBGST

  SUBROUTINE CHBGST(VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X, &
    & LDX, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(IN) :: BB(LDBB, *)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*), X(LDX, *)
  END SUBROUTINE CHBGST

  SUBROUTINE ZHBGST(VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X, &
    & LDX, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(IN) :: BB(LDBB, *)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*), X(LDX, *)
  END SUBROUTINE ZHBGST

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBTRD

  SUBROUTINE CHBTRD(VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ, &
    & WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: D(*), E(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), Q(LDQ, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CHBTRD

  SUBROUTINE ZHBTRD(VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ, &
    & WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: D(*), E(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), Q(LDQ, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZHBTRD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBGVX

  SUBROUTINE CHBGVX(JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
  &  LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
  &  LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE CHBGVX

  SUBROUTINE ZHBGVX(JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
  &  LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
  &  LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE ZHBGVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBGVD

  SUBROUTINE CHBGVD(JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
  &  W, Z, LDZ, WORK, LWORK, RWORK, LRWORK, IWORK, &
  &  LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
    INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHBGVD

  SUBROUTINE ZHBGVD(JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
  &  W, Z, LDZ, WORK, LWORK, RWORK, LRWORK, IWORK, &
  &  LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
    INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHBGVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBGV

  SUBROUTINE CHBGV(JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W, &
  & Z, LDZ, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHBGV

  SUBROUTINE ZHBGV(JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W, &
  & Z, LDZ, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHBGV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBEVX

  SUBROUTINE CHBEVX(JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, &
  &  VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, &
  &  RWORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Q(LDQ, *), Z(LDZ, *), WORK(*)
  END SUBROUTINE CHBEVX

  SUBROUTINE ZHBEVX(JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, &
  &  VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, &
  &  RWORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Q(LDQ, *), Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHBEVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBEVD

  SUBROUTINE CHBEVD(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
  &  LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK, LRWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHBEVD

  SUBROUTINE ZHBEVD(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
  &  LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK, LRWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHBEVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HBEV

  SUBROUTINE CHBEV(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
  & RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE CHBEV

  SUBROUTINE ZHBEV(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
  & RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
    COMPLEX(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE ZHBEV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_HB_LAPACK
