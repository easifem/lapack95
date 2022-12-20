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

SUBROUTINE ZLAPMR_F95(X, K, FORWRD)
  USE LA_PRECISION, ONLY: WP => DP
  USE F77_LAPACK, ONLY: LAPMR_F77 => LA_LAPMR
  INTEGER, INTENT(INOUT) :: K(:)
  !! K is INTEGER array, dimension (M)
  !! On entry, K contains the permutation vector. K is used as
  !! internal workspace, but reset to its original value on
  !! output.
  COMPLEX(WP), INTENT(INOUT) :: X(:, :)
  !! X is REAL array, dimension (LDX,N)
  !! On entry, the M by N matrix X.
  !! On exit, X contains the permuted matrix X.
  LOGICAL, OPTIONAL, INTENT(IN) :: FORWRD
  !! FORWRD is LOGICAL
  !! = .TRUE., forward permutation
  !! = .FALSE., backward permutation
  !! DEFAULT is .TRUE.
  !
  ! Internal variables
  !
  INTEGER :: M, N, LDX
  LOGICAL :: LFORWRD
  !
  IF (PRESENT(FORWRD)) THEN
    LFORWRD = FORWRD
  ELSE
    LFORWRD = .TRUE.
  END IF
  !
  M = SIZE(X, 1)
  N = SIZE(X, 2)
  LDX = MAX(1, M)
  !
  CALL LAPMR_F77(LFORWRD, M, N, X, LDX, K)
  !
END SUBROUTINE ZLAPMR_F95
