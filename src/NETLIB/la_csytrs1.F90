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
! summary: SSYTRS
!
!# Introduction
!
!
! Purpose
! =======
!
! LA_GETRS solves a system of linear equations
!    A X = B, A^T X = B or  A^H X = B
! with a general square matrix A using the LU factorization computed
! by LA_GETRF.
!
! Arguments
! =========
! SUBROUTINE LA_GETRS (A, IPIV, B, TRANS, INFO)
!    <type>(<wp>), INTENT(IN)  :: A(:,:)
!    <type>(<wp>), INTENT(INOUT) :: <rhs>
!    INTEGER, INTENT(IN) :: IPIV(:)
!    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
!    INTEGER, INTENT(OUT), OPTIONAL :: INFO
!    where
!    <type> ::= REAL | COMPLEX
!    <wp>   ::= KIND(1.0) | KIND(1.0D0)
!    <rhs>  ::= B(:,:) | B(:)
!
! =====================
!
! A     (input) either REAL or COMPLEX square array,
!       shape (:,:), size(A,1) == size(A,2).
!       The factors L and U from the factorization A = PLU as computed
!       by LA_GETRF.
!
! IPIV  (input) INTEGER array, shape (:), size(IPIV) == size(A,1).
!       The pivot indices from LA_GETRF; for 1<=i<=size(A,1), row i
!       of the matrix was interchanged with row IPIV(i).
!
! B     (input/output) either REAL or COMPLEX rectangular array,
!       shape either (:,:) or (:), size(B,1) or size(B) == size(A,1).
!       On entry, the right hand side vector(s) of matrix B for
!          the system of equations AX = B.
!       On exit, if there is no error, the matrix of solution
!          vector(s) X.
!
! TRANS Optional (input) CHARACTER*1
!       If TRANS is present, it specifies the form of the system
!          of equations:
!          = 'N':  A X = B    (No transpose)
!          = 'T':  A^T X = B  (Transpose)
!          = 'C':  A^H X = B  (Conjugate transpose = Transpose)
!       otherwise TRANS = 'N' is assumed.
!
! INFO  Optional (output) INTEGER.
!       If INFO is present
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!       If INFO is not present and an error occurs, then the program is
!          terminated with an error message.

SUBROUTINE CSYTRS1_F95(A, B, IPIV, UPLO, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  USE LA_AUXMOD, ONLY: LSAME, ERINFO
  USE F77_LAPACK, ONLY: SYTRS_F77 => LA_SYTRS
  IMPLICIT NONE
  !!
  COMPLEX(WP), INTENT(INOUT) :: A(:, :), B(:)
  !! A is returned by SYTRF
  INTEGER, INTENT(IN) :: IPIV(:)
  !! permutation info retruned by SYTRF
  CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
  !! Default is "U"
  INTEGER, INTENT(OUT), OPTIONAL :: INFO
  !! error info
  !
  ! Local variables
  !
  CHARACTER(LEN=*), PARAMETER :: SRNAME = 'LA_SYTRS'
  CHARACTER(LEN=1) :: LUPLO
  INTEGER :: LINFO, NRHS, N, LD
  !
  ! main computation
  !
  LINFO = 0
  N = SIZE(A, 1)
  NRHS = 1
  LD = MAX(1, N)
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
  ELSE IF (SIZE(B, 1) .NE. N) THEN
    LINFO = -3
  ELSE IF (.NOT. LSAME(LUPLO, 'U') .AND. .NOT. LSAME(LUPLO, 'L')) THEN
    LINFO = -4
  ELSE
    CALL SYTRS_F77(LUPLO, N, NRHS, A, LD, IPIV, B, LD, LINFO)
  END IF
  CALL ERINFO(LINFO, SRNAME, INFO)
END SUBROUTINE CSYTRS1_F95
