# This program is a part of EASIFEM library
# Copyright (C) 2020-2023  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#
#
# if(installRequest("OpenBLAS")):
#   print("====================================")
#   cwd = os.getcwd()
#   extpkgs_dir = os.getenv('HOME') + "/easifem-extpkgs"
#   os.makedirs(extpkgs_dir, exist_ok=True)
#   os.chdir(extpkgs_dir)
#   os.system(
#       f"git clone --branch develop https://github.com/xianyi/OpenBLAS.git")
#   os.chdir(extpkgs_dir + "/OpenBLAS")
#   openblas_def = "-S ./ -B build -DCMAKE_INSTALL_PREFIX=${EASIFEM_EXTPKGS}
# -DBUILD_WITHOUT_LAPACK=OFF -DBUILD_WITHOUT_CBLAS=ON
# -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -DNOFORTRAN=OFF"
#   os.system(f"cmake {openblas_def}")
#   os.chdir(cwd)
#   print("====================================")

import os
import platform

print("Detecting OS type...")
_os = platform.system()
if _os == "Windows":
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    # print("Please use Windows Subsystem Linux(WSL) ")
    # print("Installation DONE!!")
else:
    cmake_def = ""
    cmake_def += ' -G "Ninja" '  # Unix Makefiles, Ninja, Ninja Multi-Config
    cmake_def += "-D USE_OpenMP:BOOL=ON "
    cmake_def += "-D CMAKE_BUILD_TYPE:STRING=Release "
    cmake_def += "-D BUILD_SHARED_LIBS:BOOL=ON "
    cmake_def += "-D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_EXTPKGS} "

    print("CMAKE DEF : ", cmake_def)
    _build0 = os.path.join(os.environ["HOME"], "temp")
    build_dir = os.path.join(
        os.environ.get("EASIFEM_BUILD_DIR", _build0),
        "easifem",
        "extpkgs",
        "build",
    )
    os.makedirs(build_dir, exist_ok=True)
    os.system(f"cmake -S ./ -B {build_dir} {cmake_def}")
    os.system(f"cmake --build {build_dir} --target install")
    print("Installation DONE!!")
