# syntax=docker/dockerfile:1

FROM ubuntu:latest as system_builder
USER root
ENV EASIFEM_BUILD_DIR /easifem/build
ENV EASIFEM_SOURCE_DIR /easifem/src
ENV EASIFEM_INSTALL_DIR /easifem/install
ENV EASIFEM_TEST_DIR /easifem/tests
ENV EASIFEM_EXTPKGS $EASIFEM_INSTALL_DIR/easifem/extpkgs

RUN --mount=type=cache,target=/root/.cache apt-get update && \
apt-get install -y --no-install-recommends \
gfortran \
gcc \
g++ \
libomp-dev \
cmake \
ninja-build \
liblapack-dev \
libopenblas-dev \
&& apt-get clean 
RUN mkdir -pv \
$EASIFEM_EXTPKGS/include \
$EASIFEM_EXTPKGS/lib \
$EASIFEM_EXTPKGS/bin

FROM system_builder

COPY . $EASIFEM_SOURCE_DIR/extpkgs/LAPACK95

WORKDIR $EASIFEM_SOURCE_DIR/extpkgs/LAPACK95

RUN cmake -S . -B $EASIFEM_BUILD_DIR/easifem/extpkgs/build -G Ninja \
-D USE_OpenMP:BOOL=ON \
-D CMAKE_BUILD_TYPE:STRING=Release \
-D BUILD_SHARED_LIBS:BOOL=ON \
-D CMAKE_INSTALL_PREFIX:PATH=$EASIFEM_EXTPKGS \
&& cmake --build \
$EASIFEM_BUILD_DIR/easifem/extpkgs/build --target install 

# RUN gfortran -o main.out main.F90 && ./main.out


