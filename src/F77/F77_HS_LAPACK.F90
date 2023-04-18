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

MODULE F77_HS_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HSEIN

  SUBROUTINE SHSEIN(SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,&
 &                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,       &
 &                   IFAILR, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
    INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(INOUT) :: WR(*), WI(*)
    REAL(WP), INTENT(IN) :: H(LDH, *)
    REAL(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE SHSEIN

  SUBROUTINE DHSEIN(SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,&
 &                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,       &
 &                   IFAILR, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
    INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(INOUT) :: WR(*), WI(*)
    REAL(WP), INTENT(IN) :: H(LDH, *)
    REAL(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *)
    REAL(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE DHSEIN

  SUBROUTINE CHSEIN(SIDE, EIGSRC, INITV, SELECT, N, H, LDH, W, VL, &
 &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL,    &
 &                   IFAILR, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
    INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(IN) :: H(LDH, *)
    COMPLEX(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *), W(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE CHSEIN

  SUBROUTINE ZHSEIN(SIDE, EIGSRC, INITV, SELECT, N, H, LDH, W, VL, &
 &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL,    &
 &                   IFAILR, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
    INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
    INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
    LOGICAL, INTENT(IN) :: SELECT(*)
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(IN) :: H(LDH, *)
    COMPLEX(WP), INTENT(INOUT) :: VL(LDVL, *), VR(LDVR, *), W(*)
    COMPLEX(WP), INTENT(OUT) :: WORK(*)
  END SUBROUTINE ZHSEIN

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HSEQR

  SUBROUTINE SHSEQR(JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,    &
 &                   LDZ, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: WR(*), WI(*)
    REAL(WP), INTENT(INOUT) :: H(LDH, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE SHSEQR

  SUBROUTINE DHSEQR(JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,    &
 &                   LDZ, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: WR(*), WI(*)
    REAL(WP), INTENT(INOUT) :: H(LDH, *), Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: WORK(LWORK)
  END SUBROUTINE DHSEQR

  SUBROUTINE CHSEQR(JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,    &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: H(LDH, *), Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: W(*), WORK(LWORK)
  END SUBROUTINE CHSEQR

  SUBROUTINE ZHSEQR(JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,    &
 &                   WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: H(LDH, *), Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: W(*), WORK(LWORK)
  END SUBROUTINE ZHSEQR

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_HS_LAPACK
