# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2022, All right reserved, Vikas Sharma, Ph.D.
#

import os
import sys
import platform


def installRequest(LIB):
    while True:
        choice = input(f"Do you want to Install {LIB} 'yes' or 'no' [Y/n]: ")
        if choice == " ":
            choice = "no"
        else:
            choice = choice.lower()
        if choice in ["Y", "y", "ye", "yes"]:
            return True
        else:
            return False


def getOption(key, opt):
    while True:
        separator = ", "
        return (
            input(
                f"select option for {key}, possible options are : {separator.join(opt)} : "
            )
            + " "
        )


print("Detecting OS type...")
_os = platform.system()
if _os == "Windows":
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    # print("Please use Windows Subsystem Linux(WSL) ")
    # print("Installation DONE!!")
else:
    # if(installRequest("OpenBLAS")):
    #   print("====================================")
    #   cwd = os.getcwd()
    #   extpkgs_dir = os.getenv('HOME') + "/easifem-extpkgs"
    #   os.makedirs(extpkgs_dir, exist_ok=True)
    #   os.chdir(extpkgs_dir)
    #   os.system(
    #       f"git clone --branch develop https://github.com/xianyi/OpenBLAS.git")
    #   os.chdir(extpkgs_dir + "/OpenBLAS")
    #   openblas_def = "-S ./ -B build -DCMAKE_INSTALL_PREFIX=${EASIFEM_EXTPKGS} -DBUILD_WITHOUT_LAPACK=OFF -DBUILD_WITHOUT_CBLAS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -DNOFORTRAN=OFF"
    #   os.system(f"cmake {openblas_def}")
    #   os.chdir(cwd)
    #   print("====================================")

    user_query = False
    if user_query:
        cmake_def = ""
        opt = getOption("USE_OpenMP", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -D USE_OpenMP=" + opt
        #
        #
        #
        opt = getOption("CMAKE_BUILD_TYPE", ["Release", "Debug"])
        if opt == " ":
            opt = "Release"
        cmake_def += " -D CMAKE_BUILD_TYPE=" + opt
        #
        #
        #
        opt = getOption("BUILD_SHARED_LIBS", ["ON", "OFF"])
        if opt == " ":
            opt = "ON"
        cmake_def += " -D BUILD_SHARED_LIBS=" + opt
        #
        #
        #
        opt = getOption("CMAKE_INSTALL_PREFIX", ["${PREFIX}"])
        if opt == " ":
            #   opt = "${HOME}/PENF"
            opt = "${EASIFEM_EXTPKGS}"
        cmake_def += " -D CMAKE_INSTALL_PREFIX=" + opt
        #
        #
        #
        cmake_def += " -D USE_Int32=ON -D USE_Real64=ON"
        #
        #
        #
    else:
        cmake_def = ' -G "Ninja" -D USE_OpenMP:BOOL=ON -D CMAKE_BUILD_TYPE:STRING=Release -D BUILD_SHARED_LIBS:BOOL=ON  -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_EXTPKGS} -D USE_Int32:BOOL=ON -D USE_Real64:BOOL=ON'

    print("CMAKE DEF : ", cmake_def)

    os.system(f"cmake -S ./ -B ~/temp/easifem-extpkgs/build {cmake_def}")
    os.system(f"cmake --build ~/temp/easifem-extpkgs/build --target install")
    print("Installation DONE!!")
