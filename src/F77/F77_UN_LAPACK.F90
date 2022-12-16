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

MODULE F77_UN_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMRZ

  SUBROUTINE CUNMRZ(SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
&                      LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMRZ

  SUBROUTINE ZUNMRZ(SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
&                      LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMRZ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMBR

  SUBROUTINE CUNMBR(VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
 &                   LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMBR

  SUBROUTINE ZUNMBR(VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
 &                   LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMBR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGBR

  SUBROUTINE CUNGBR(VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
 &                   INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: VECT
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGBR

  SUBROUTINE ZUNGBR(VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
 &                   INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: VECT
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGBR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMHR

  SUBROUTINE CUNMHR(SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
 &                   LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMHR

  SUBROUTINE ZUNMHR(SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
 &                   LDC, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMHR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGHR

  SUBROUTINE CUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGHR

  SUBROUTINE ZUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGHR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMTR

  SUBROUTINE CUNMTR(SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
    INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
  END SUBROUTINE CUNMTR

  SUBROUTINE ZUNMTR(SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
    INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
  END SUBROUTINE ZUNMTR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMRQ

  SUBROUTINE CUNMRQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMRQ

  SUBROUTINE ZUNMRQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMRQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGRQ

  SUBROUTINE CUNGRQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGRQ

  SUBROUTINE ZUNGRQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGRQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMQL

  SUBROUTINE CUNMQL(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMQL

  SUBROUTINE ZUNMQL(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMQL

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGQL

  SUBROUTINE CUNGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGQL

  SUBROUTINE ZUNGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGQL

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMLQ

  SUBROUTINE CUNMLQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMLQ

  SUBROUTINE ZUNMLQ(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMLQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGLQ

  SUBROUTINE CUNGLQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGLQ

  SUBROUTINE ZUNGLQ(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGLQ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNMQR

  SUBROUTINE CUNMQR(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNMQR

  SUBROUTINE ZUNMQR(SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
    INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: A(LDA, *), TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: C(LDC, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNMQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGQR

  SUBROUTINE CUNGQR(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGQR

  SUBROUTINE ZUNGQR(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_UNGTR

  SUBROUTINE CUNGTR(UPLO, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE CUNGTR

  SUBROUTINE ZUNGTR(UPLO, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: UPLO
    INTEGER, INTENT(IN) :: LDA, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(IN) :: TAU(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE ZUNGTR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_UN_LAPACK
