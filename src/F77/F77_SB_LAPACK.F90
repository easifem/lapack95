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

MODULE F77_SB_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBGST

  SUBROUTINE SSBGST(VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
 &                   LDX, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: BB(LDBB, *)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: WORK(*), X(LDX, *)
  END SUBROUTINE SSBGST

  SUBROUTINE DSBGST(VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
 &                   LDX, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: BB(LDBB, *)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: WORK(*), X(LDX, *)
  END SUBROUTINE DSBGST

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBTRD

  SUBROUTINE SSBTRD(VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
 &                   WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: D(*), E(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), Q(LDQ, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SSBTRD

  SUBROUTINE DSBTRD(VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
 &                   WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
    INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: D(*), E(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), Q(LDQ, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DSBTRD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBGVX

  SUBROUTINE SSBGVX(JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
&                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
&                    LDZ, WORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: WORK(*), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE SSBGVX

  SUBROUTINE DSBGVX(JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
&                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
&                    LDZ, WORK, IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: WORK(*), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE DSBGVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBGVD

  SUBROUTINE SSBGVD(JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
&                     W, Z, LDZ, WORK, LWORK, IWORK, LIWORK,       &
&                     INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
    INTEGER, INTENT(IN) :: LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSBGVD

  SUBROUTINE DSBGVD(JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
&                     W, Z, LDZ, WORK, LWORK, IWORK, LIWORK,       &
&                     INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
    INTEGER, INTENT(IN) :: LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSBGVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBGV

  SUBROUTINE SSBGV(JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
&                   Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSBGV

  SUBROUTINE DSBGV(JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
&                   Z, LDZ, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *), BB(LDBB, *)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSBGV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SPGVX

  SUBROUTINE SSPGVX(ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
&                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,    &
&                    IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE SSPGVX

  SUBROUTINE DSPGVX(ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
&                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,    &
&                    IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
    INTEGER, INTENT(OUT) :: M
    REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
    INTEGER, INTENT(OUT) :: IWORK(*)
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: W(*)
    INTEGER, INTENT(IN) :: IFAIL(*)
  END SUBROUTINE DSPGVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SPGVD

  SUBROUTINE SSPGVD(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ,     &
&                     WORK, LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(IN) :: LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSPGVD

  SUBROUTINE DSPGVD(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ,     &
&                     WORK, LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(IN) :: LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSPGVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SPGV

  SUBROUTINE SSPGV(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
&                   INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSPGV

  SUBROUTINE DSPGV(ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
&                   INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: ITYPE, N, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSPGV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBEVX

  SUBROUTINE SSBEVX(JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
&                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
&                    IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Q(LDQ, *), Z(LDZ, *), WORK(*)
  END SUBROUTINE SSBEVX

  SUBROUTINE DSBEVX(JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
&                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
&                    IWORK, IFAIL, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
    INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
    INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
    REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Q(LDQ, *), Z(LDZ, *), WORK(*)
  END SUBROUTINE DSBEVX

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBEVD

  SUBROUTINE SSBEVD(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
&                    LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE SSBEVD

  SUBROUTINE DSBEVD(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
&                    LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*)
    REAL(WP), INTENT(OUT) :: Z(LDZ, *), WORK(*)
  END SUBROUTINE DSBEVD

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_SBEV

  SUBROUTINE SSBEV(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
&                   INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), Z(LDZ, *), WORK(*)
  END SUBROUTINE SSBEV

  SUBROUTINE DSBEV(JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
&                   INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
    INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: AB(LDAB, *)
    REAL(WP), INTENT(OUT) :: W(*), Z(LDZ, *), WORK(*)
  END SUBROUTINE DSBEV

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_SB_LAPACK
