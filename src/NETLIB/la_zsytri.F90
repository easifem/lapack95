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

!> author: Vikas Sharma, Ph. D.
! date:         2022-12-20
! summary: SYTRI

SUBROUTINE ZSYTRI_F95(A, IPIV, UPLO, INFO)
  USE LA_PRECISION, ONLY: WP => DP
  USE LA_AUXMOD, ONLY: LSAME, ERINFO
  USE F77_LAPACK, ONLY: SYTRI_F77 => LA_SYTRI
  IMPLICIT NONE
  !!
  COMPLEX(WP), INTENT(INOUT) :: A(:, :)
  !! A is returned by SYTRF
  INTEGER, INTENT(IN) :: IPIV(:)
  !! permutation info retruned by SYTRF
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: UPLO
  !! Default is "U"
  INTEGER, OPTIONAL, INTENT(OUT) :: INFO
  !! error info
  !
  ! Local variables
  !
  CHARACTER(LEN=*), PARAMETER :: SRNAME = 'LA_SYTRI'
  CHARACTER(LEN=1) :: LUPLO
  INTEGER :: LINFO, N, LDA
  COMPLEX(WP), ALLOCATABLE :: WORK(:)
  !
  ! main computation
  !
  LINFO = 0
  N = SIZE(A, 1)
  LDA = MAX(1, N)
  IF (PRESENT(UPLO)) THEN
    LUPLO = UPLO
  ELSE
    LUPLO = 'U'
  END IF
  !
  IF (SIZE(A, 2) .NE. N .OR. N < 0) THEN
    LINFO = -1
  ELSE IF (SIZE(IPIV) .NE. N) THEN
    LINFO = -2
  ELSE IF (.NOT. LSAME(LUPLO, 'U') .AND. .NOT. LSAME(LUPLO, 'L')) THEN
    LINFO = -4
  ELSE
    ALLOCATE (WORK(N))
    CALL SYTRI_F77(LUPLO, N, A, LDA, IPIV, WORK, LINFO)
    DEALLOCATE (WORK)
  END IF
  CALL ERINFO(LINFO, SRNAME, INFO)
END SUBROUTINE ZSYTRI_F95
