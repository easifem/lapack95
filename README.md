# LAPACK95

- [LAPACK95](#lapack95)
      - [The HTML version of the LAPACK95 Users' Guide is now available, [here](http://www.netlib.org/lapack95/lug95/)](#the-html-version-of-the-lapack95-users-guide-is-now-available-herehttpwwwnetliborglapack95lug95)
      - [Index of LAPACK95 routines is [here](http://www.netlib.org/lapack95/L90index/L90index.html)](#index-of-lapack95-routines-is-herehttpwwwnetliborglapack95l90indexl90indexhtml)
  - [Build](#build)
    - [CMake](#cmake)
    - [Python](#python)
  - [Examples](#examples)

LAPACK95 is a Fortran95 interface to LAPACK. This repository provides a facility to build LAPACK95 by using CMake build system. This repository is build mainly for satisfying the dependencies to build [EASIFEM](www.easifem.com) library. I would recommend you to use scivision's Lapack95 library instead of this one, which is avaiable [here](https://github.com/scivision/LAPACK95).

#### The HTML version of the LAPACK95 Users' Guide is now available, [here](http://www.netlib.org/lapack95/lug95/)

#### Index of LAPACK95 routines is [here](http://www.netlib.org/lapack95/L90index/L90index.html)

## Build

- The build process produces library called `liblapack95` and modules. The archive library can be shared or static.
- The library is build for single and double precision.
- Complex data type is not included.

### CMake

Following options are defined:

- `-DUSE_OPENMP=ON/OFF` : to enable and disable openmp
- `-DCMAKE_BUILD_TYPE=Release/Debug` for release or debug type build
- `BUILD_SHARED_LIBS=ON/OFF` to build shared or static lib
- `-DCMAKE_INSTALL_PREFIX`, location of the installion directory

```sh
git clone https://github.com/vickysharma0812/LAPACK95.git
cd LAPACK95
cmake -DUSE_OPENMP=ON -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX=${EASIFEM_EXTPKGS} -S ./ -B ./build
cmake --build ./build --target install
```

### Python

```sh
git clone https://github.com/vickysharma0812/LAPACK95.git
cd LAPACK95
python3 install.py
```

## Examples

```fortran
! Double precision
use la_precision, only: wp => dp
use f95_lapack, only: la_gesv

real(wp) :: A(3,3), b(3)

call random_number(A)
b(:) = 3*A(:,1) + 2*A(:,2) - A(:,3)

! Solve Ax=b, overwrite b with solution
call la_gesv(A,b)

print *, b
end program
```
