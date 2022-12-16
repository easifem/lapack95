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

MODULE F77_HG_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_HGEQZ

  SUBROUTINE SHGEQZ(JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
 &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,    &
 &                   LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),       &
&                              Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*), WORK(LWORK)
  END SUBROUTINE SHGEQZ

  SUBROUTINE DHGEQZ(JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
 &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,    &
 &                   LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),       &
&                              Z(LDZ, *)
    REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*), WORK(LWORK)
  END SUBROUTINE DHGEQZ

  SUBROUTINE CHGEQZ(JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
 &                   ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,      &
 &                   RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),    &
&                                 Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
  END SUBROUTINE CHGEQZ

  SUBROUTINE ZHGEQZ(JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
 &                   ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,      &
 &                   RWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
    INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(OUT) :: RWORK(*)
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *), B(LDB, *), Q(LDQ, *),    &
&                                 Z(LDZ, *)
    COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
  END SUBROUTINE ZHGEQZ

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_HG_LAPACK
