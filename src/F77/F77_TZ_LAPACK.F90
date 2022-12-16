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

MODULE F77_TZ_LAPACK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TZRZF

  SUBROUTINE STZRZF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE STZRZF

  SUBROUTINE DTZRZF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE DTZRZF

  SUBROUTINE CTZRZF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE CTZRZF

  SUBROUTINE ZTZRZF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE ZTZRZF

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LA_TZRQF

  SUBROUTINE STZRQF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE STZRQF

  SUBROUTINE DTZRQF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    REAL(WP), INTENT(INOUT) :: A(LDA, *)
    REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE DTZRQF

  SUBROUTINE CTZRQF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => SP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE CTZRQF

  SUBROUTINE ZTZRQF(M, N, A, LDA, TAU, WORK, LWORK, INFO)
    USE LA_PRECISION, ONLY: WP => DP
    INTEGER, INTENT(IN) :: LDA, LWORK, M, N
    INTEGER, INTENT(OUT) :: INFO
    COMPLEX(WP), INTENT(INOUT) :: A(LDA, *)
    COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
  END SUBROUTINE ZTZRQF

END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE F77_TZ_LAPACK
