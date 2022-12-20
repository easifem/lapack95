!
!> author: Vikas Sharma, Ph. D.
! date: 2022-12-16
! summary: CSYTRF
!
!# Introduction
!
!# Purpose
!
! LA_SYTRF computes the factorization of a real symmetric matrix A using
! the Bunch-Kaufman diagonal pivoting method.  The form of the
! factorization is
!
!   A = U**T*D*U  or  A = L*D*L**T
!
! where U (or L) is a product of permutation and unit upper (lower)
! triangular matrices, and D is symmetric and block diagonal with
! 1-by-1 and 2-by-2 diagonal blocks.
!
! This is the blocked version of the algorithm, calling Level 3 BLAS.
!
! LA_SYTRF optionally estimates the reciprocal of the condition number
! (in the 1-norm) of a real symmetric or complex Hermitian positive
! definite matrix A.
! An estimate is obtained for norm(inv(A)), and the reciprocal of the
! condition number is computed as RCOND = 1 / (norm(A) * norm(inv(A))).
!
! =======
!
!    SUBROUTINE LA_SYTRF( A, UPLO, RCOND, NORM, INFO )
!       <type>(<wp>), INTENT(INOUT) :: A(:,:)
!       CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
!       REAL(<wp>), INTENT(OUT), OPTIONAL :: RCOND
!       CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM
!       INTEGER, INTENT(OUT), OPTIONAL :: INFO
!    where
!       <type> ::= REAL | COMPLEX
!       <wp>   ::= KIND(1.0) | KIND(1.0D0)
!
! Defaults
! ========
!
! 1. If UPLO is not present then UPLO = 'U' is assumed.
!
!## Arguments
!
!### A
! (input/output) either REAL or COMPLEX square array,
!         shape (:,:), size(A,1) == size(A,2) >= 0.
!         On entry, the symmetric (Hermitian) matrix A.
!            If UPLO = 'U', the upper triangular part of A contains
!               the upper triangular part of the matrix A, and the
!               strictly lower triangular part of A is not referenced.
!            If UPLO = 'L', the lower triangular part of A contains
!               the lower triangular part of the matrix A, and the
!               strictly upper triangular part of A is not referenced.
!         On exit, the block diagonal matrix D and the multipliers used
!         to obtain the factor U or L (see below for further details).
!
!### UPLO
!
! Optional, (input) CHARACTER*1
!         If UPLO is present then:
!            = 'U':  Upper triangle of A is stored;
!            = 'L':  Lower triangle of A is stored.
!         otherwise UPLO = 'U' is assumed.
!
!### IPIV
!
! Optional (output) INTEGER array,
!        shape (:) with size(IPIV) = size(A,1).
!        IPIV is INTEGER array, dimension (N)
!        Details of the interchanges and the block structure of D.
!        If IPIV(k) > 0, then rows and columns k and IPIV(k) were
!        interchanged and D(k,k) is a 1-by-1 diagonal block.
!        If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and
!        columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
!        is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =
!        IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
!        interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
!        The pivot indices that define the permutation matrix P; row i of
!        the matrix was interchanged with row IPIV(i).
!
!### RCOND
! Optional (output) REAL
!         The reciprocal of the condition number of the matrix A
!         computed as RCOND = 1/(norm(A) * norm(inv(A))).
!
!### NORM
! Optional (input) CHARACTER*1
!         Specifies whether the 1-norm condition number or the
!         infinity-norm condition number is required:
!
!           If NORM is present then:
!              = '1', 'O' or 'o': 1-norm;
!              = 'I' or 'i': infinity-norm.
!           otherwise NORM = '1' is used.
!
!
!### INFO
! Optional, (output) INTEGER
!         If INFO is present:
!         INFO is INTEGER
!         = 0:  successful exit
!         < 0:  if INFO = -i, the i-th argument had an illegal value
!         > 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
!               has been completed, but the block diagonal matrix D is
!               exactly singular, and division by zero will occur if it
!               is used to solve a system of equations

SUBROUTINE CSYTRF_F95(A, UPLO, IPIV, RCOND, NORM, INFO)
  USE LA_PRECISION, ONLY: WP => SP
  USE LA_AUXMOD, ONLY: ERINFO, LSAME
  USE F77_LAPACK, ONLY: SYTRF_F77 => LA_SYTRF, &
                        LANSY_F77 => LA_LANSY, &
                        SYCON_F77 => LA_SYCON
  IMPLICIT NONE
  ! CHARACTER ARGUMENTS ..
  CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: NORM, UPLO
  ! SCALAR ARGUMENTS ..
  INTEGER, INTENT(OUT), OPTIONAL :: INFO
  REAL(WP), INTENT(OUT), OPTIONAL :: RCOND
  ! ARRAY ARGUMENTS ..
  INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IPIV(:)
  COMPLEX(WP), INTENT(INOUT) :: A(:, :)
  !
  ! LOCAL PARAMETERS
  !
  CHARACTER(LEN=*), PARAMETER :: SRNAME = 'LA_SYTRF'
  !
  !  LOCAL SCALARS
  !
  CHARACTER(LEN=1) :: LNORM, LUPLO
  INTEGER :: LINFO, N, ISTAT, ISTAT1, LD, LWORK, SIPIV
  REAL(WP) :: ANORM
  !
  ! LOCAL POINTERS
  !
  INTEGER, POINTER :: IWORK(:), LIPIV(:)
  COMPLEX(WP), POINTER :: WORK(:)
  REAL(WP), POINTER :: RWORK(:)
  !
  ! INTRINSIC FUNCTIONS
  !
  INTRINSIC PRESENT, MAX
  !
  ! EXECUTABLE STATEMENTS
  !
  LINFO = 0
  N = SIZE(A, 1)
  LD = MAX(1, N)
  ISTAT = 0
  !
  IF (PRESENT(IPIV)) THEN
    SIPIV = SIZE(IPIV)
  ELSE
    SIPIV = N
  END IF
  !
  IF (PRESENT(UPLO)) THEN
    LUPLO = UPLO
  ELSE
    LUPLO = 'U'
  END IF
  !
  IF (PRESENT(NORM)) THEN
    LNORM = NORM
  ELSE
    LNORM = '1'
  END IF
  !
  ! TEST THE ARGUMENTS
  !
  IF ((SIZE(A, 2) .NE. N) .AND. (N .LT. 0)) THEN
    LINFO = -1
  ELSE IF ((.NOT. LSAME(LUPLO, 'U')) .AND. (.NOT. LSAME(LUPLO, 'L'))) THEN
    LINFO = -2
  ELSE IF ((.NOT. PRESENT(RCOND) .AND. PRESENT(NORM)) .OR. &
    & (.NOT. LSAME(LNORM, 'I') .AND. .NOT. LSAME(LNORM, 'O') &
    & .AND. LNORM /= '1')) THEN
    LINFO = -4
  ELSE IF (N > 0) THEN
    IF (PRESENT(RCOND)) THEN
      !
      ! COMPUTE THE NORM OF THE MATRIX A
      !
      ALLOCATE (RWORK(N), STAT=ISTAT)
      IF (ISTAT .EQ. 0) THEN
        ANORM = LANSY_F77(LNORM, LUPLO, LD, A, N, RWORK)
      ELSE
        LINFO = -100
      END IF
      DEALLOCATE (RWORK, STAT=ISTAT1)
    END IF
    !
    IF (LINFO .EQ. 0) THEN
      IF (PRESENT(IPIV)) THEN
        LIPIV => IPIV
      ELSE
        ALLOCATE (LIPIV(N), STAT=ISTAT)
      END IF
      IF (ISTAT .NE. 0) LINFO = -100
    END IF
    !
    IF (LINFO .EQ. 0) THEN
      !
      ! COMPUTE THE LDLt FACTORS OF THE MATRIX A
      !
      LWORK = -1
      !
      ! Get the LWORK by calling SYTRF with LWORK=-1
      !
      ALLOCATE (WORK(1), STAT=ISTAT)
      IF (ISTAT .EQ. 0) THEN
        !
        CALL SYTRF_F77(LUPLO, N, A, LD, LIPIV, WORK, LWORK, LINFO)
        !
        IF (LINFO .EQ. 0) THEN
          LWORK = INT(WORK(1))
          DEALLOCATE (WORK)
          ALLOCATE (WORK(LWORK))
          CALL SYTRF_F77(LUPLO, N, A, LD, LIPIV, WORK, LWORK, LINFO)
          DEALLOCATE (WORK)
        END IF
      ELSE
        LINFO = -100
        DEALLOCATE (WORK, LIPIV, STAT=ISTAT1)
      END IF
      !
      IF (PRESENT(RCOND) .AND. (LINFO .EQ. 0)) THEN
        !
        ! COMPUTE THE RECIPROCAL OF THE CONDITION NUMBER OF A
        !
        IF (ANORM .EQ. 0.0_WP) THEN
          RCOND = 0.0_WP
          DEALLOCATE (LIPIV)
        ELSE
          ALLOCATE (WORK(2 * N), IWORK(N), STAT=ISTAT)
          IF (ISTAT .EQ. 0) THEN
            CALL SYCON_F77(LUPLO, N, A, LD, LIPIV, ANORM, RCOND, &
              & WORK, LINFO)
          ELSE
            LINFO = -100
          END IF
          IF (.NOT. PRESENT(IPIV)) THEN
            DEALLOCATE (WORK, IWORK, LIPIV, STAT=ISTAT1)
          ELSE
            DEALLOCATE (WORK, IWORK, STAT=ISTAT1)
          END IF
        END IF
      END IF
    END IF
  ELSE IF (PRESENT(RCOND)) THEN
    RCOND = 1.0_WP
  END IF
  !
  CALL ERINFO(LINFO, SRNAME, INFO, ISTAT)
  !
END SUBROUTINE CSYTRF_F95
