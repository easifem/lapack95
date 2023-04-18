!
!> author: Vikas Sharma, Ph. D.
! date: 2022-12-16
! summary: SSYTRF
!
!# Introduction
!
!# Purpose
!
!  CLACPY copies all or part of a two-dimensional matrix A to another
!  matrix B.
!
! 1. If UPLO is not present then UPLO = 'A' is assumed.

SUBROUTINE CLACPY_F95(A, B, UPLO)
  USE LA_PRECISION, ONLY: WP => SP
  USE F77_LAPACK, ONLY: LACPY_F77 => LA_LACPY
  IMPLICIT NONE
  ! CHARACTER ARGUMENTS ..
  CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
  ! ARRAY ARGUMENTS ..
  COMPLEX(WP), INTENT(IN) :: A(:, :)
  COMPLEX(WP), INTENT(OUT) :: B(:, :)
  !
  !  LOCAL SCALARS
  !
  CHARACTER(LEN=1) :: LUPLO
  INTEGER :: M, N, LDA, LDB
  !
  ! EXECUTABLE STATEMENTS
  !
  LDA = SIZE(A, 1)
  LDB = SIZE(B, 1)
  M = LDA
  N = SIZE(A, 2)
  !
  IF (PRESENT(UPLO)) THEN
    LUPLO = UPLO
  ELSE
    LUPLO = 'A' !! ALL entries
  END IF
  !
  CALL LACPY_F77(LUPLO, M, N, A, LDA, B, LDB)
  !
END SUBROUTINE CLACPY_F95
