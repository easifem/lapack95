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

MODULE F77_TG_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TGSEN

  SUBROUTINE STGSEN(IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
 &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, M, PL,   &
 &                   PR, DIF, WORK, LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
    REAL(WP), INTENT(OUT) :: PL, PR
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: ALPHAI(*), ALPHAR(*), BETA(*), DIF(2),   &
   &                         WORK(LWORK)
  END SUBROUTINE STGSEN

  SUBROUTINE DTGSEN(IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
 &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, M, PL,   &
 &                   PR, DIF, WORK, LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
    REAL(WP), INTENT(OUT) :: PL, PR
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: ALPHAI(*), ALPHAR(*), BETA(*), DIF(2),   &
   &                         WORK(LWORK)
  END SUBROUTINE DTGSEN

  SUBROUTINE CTGSEN(IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
 &                   ALPHA, BETA, Q, LDQ, Z, LDZ, M, PL, PR, DIF,   &
 &                   WORK, LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
    REAL(WP), INTENT(OUT) :: PL, PR
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: DIF(2)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),       &
   &                              Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
  END SUBROUTINE CTGSEN

  SUBROUTINE ZTGSEN(IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
 &                   ALPHA, BETA, Q, LDQ, Z, LDZ, M, PL, PR, DIF,   &
 &                   WORK, LWORK, IWORK, LIWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
    REAL(WP), INTENT(OUT) :: PL, PR
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: DIF(2)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),       &
   &                              Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
  END SUBROUTINE ZTGSEN

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TGSNA

  SUBROUTINE STGSNA(JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
 &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: DIF(*), S(*)
    REAL(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), VL(LDVL, *),           &
   &                        VR(LDVR, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE STGSNA

  SUBROUTINE DTGSNA(JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
    & LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
    & IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: DIF(*), S(*)
    REAL(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), VL(LDVL, *),           &
   &                        VR(LDVR, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DTGSNA

  SUBROUTINE CTGSNA(JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
 &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: DIF(*), S(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), VL(LDVL, *),        &
   &                           VR(LDVR, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CTGSNA

  SUBROUTINE ZTGSNA(JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
 &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: DIF(*), S(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), VL(LDVL, *),        &
   &                           VR(LDVR, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZTGSNA

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TGSYL

  SUBROUTINE STGSYL(TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
 &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
   &                       M, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: DIF, SCALE
    REAL(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), D(LDD, *), E(LDF, *)
    REAL(WP), INTENT(INOUT) :: C(LDC, *), F(LDF, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE STGSYL

  SUBROUTINE DTGSYL(TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
 &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
   &                       M, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: DIF, SCALE
    REAL(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), D(LDD, *), E(LDF, *)
    REAL(WP), INTENT(INOUT) :: C(LDC, *), F(LDF, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DTGSYL

  SUBROUTINE CTGSYL(TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
 &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
   &                       M, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: DIF, SCALE
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), D(LDD, *), E(LDF, *)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *), F(LDF, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CTGSYL

  SUBROUTINE ZTGSYL(TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
 &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
 &                   IWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
   &                       M, N
    INTEGER, INTENT(OUT) :: INFO, IWORK(*)
    REAL(WP), INTENT(OUT) :: DIF, SCALE
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), B(LDB, *), D(LDD, *), E(LDF, *)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *), F(LDF, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZTGSYL

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TGEXC

  SUBROUTINE STGEXC(WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
&                      LDZ, IFST, ILST, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, LWORK, N
    INTEGER, INTENT(INOUT) :: IFST, ILST
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE STGEXC

  SUBROUTINE DTGEXC(WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
&                      LDZ, IFST, ILST, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, LWORK, N
    INTEGER, INTENT(INOUT) :: IFST, ILST
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DTGEXC

  SUBROUTINE CTGEXC(WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
&                      LDZ, IFST, ILST, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, N
    INTEGER, INTENT(INOUT) :: IFST, ILST
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),    &
&                                 Z(LDZ, *)
  END SUBROUTINE CTGEXC

  SUBROUTINE ZTGEXC(WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
&                      LDZ, IFST, ILST, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    LOGICAL, INTENT(IN) :: WANTQ, WANTZ
    INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, N
    INTEGER, INTENT(INOUT) :: IFST, ILST
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),    &
&                                 Z(LDZ, *)
  END SUBROUTINE ZTGEXC

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TGSJA

  SUBROUTINE STGSJA(JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
 &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
 &                   Q, LDQ, WORK, NCYCLE, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
    INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
&                          NCYCLE, P
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TOLA, TOLB
    REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),       &
&                              U(LDU, *), V(LDV, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE STGSJA

  SUBROUTINE DTGSJA(JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
 &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
 &                   Q, LDQ, WORK, NCYCLE, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
    INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
&                          NCYCLE, P
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TOLA, TOLB
    REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),       &
&                              U(LDU, *), V(LDV, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DTGSJA

  SUBROUTINE CTGSJA(JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
 &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
 &                   Q, LDQ, WORK, NCYCLE, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
    INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
&                          NCYCLE, P
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TOLA, TOLB
    REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),    &
&                                 U(LDU, *), V(LDV, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CTGSJA

  SUBROUTINE ZTGSJA(JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
 &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
 &                   Q, LDQ, WORK, NCYCLE, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
    INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
&                          NCYCLE, P
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(IN) :: TOLA, TOLB
    REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),    &
&                                 U(LDU, *), V(LDV, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZTGSJA

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TGEVC

  SUBROUTINE STGEVC(SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
 &                   LDVL, VR, LDVR, MM, M, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(IN) :: A(LDA, *), B(LDB, *)
    REAL(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE STGEVC

  SUBROUTINE DTGEVC(SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
 &                   LDVL, VR, LDVR, MM, M, WORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(IN) :: A(LDA, *), B(LDB, *)
    REAL(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DTGEVC

  SUBROUTINE CTGEVC(SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
 &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CTGEVC

  SUBROUTINE ZTGEVC(SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
 &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
    INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), B(LDB, *)
    COMPLEX(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZTGEVC

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_TG_LAPACK
