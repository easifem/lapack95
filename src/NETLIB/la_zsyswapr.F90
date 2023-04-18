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

SUBROUTINE ZSYSWAPR_F95(A, I1, I2, UPLO)
  USE LA_PRECISION, ONLY: WP => SP
  USE F77_LAPACK, ONLY: SYSWAPR_F77 => LA_SYSWAPR
  COMPLEX(WP), INTENT(INOUT) :: A(:, :)
  INTEGER, INTENT(IN) :: I1, I2
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: UPLO
  !
  ! Internal variables
  !
  INTEGER :: N, LDA
  CHARACTER(LEN=1) :: LUPLO
  !
  IF (PRESENT(UPLO)) THEN
    LUPLO = UPLO
  ELSE
    LUPLO = "U"
  END IF
  !
  LDA = SIZE(A, 1)
  N = SIZE(A, 2)
  !
  CALL SYSWAPR_F77(LUPLO, N, A, LDA, I1, I2)
  !
END SUBROUTINE zSYSWAPR_F95
