project(
  'LAPACK95',
  'fortran',
  version: '2.1.0',
  meson_version: '>=0.55',
  default_options: [
    'warning_level=3',
    'fortran_std=f2003',
    'default_library=both',
    'libdir=lib/',
    'buildtype=debugoptimized',
    'backend=ninja',
    ],
)

project_description='LAPACK95 is a Fortran95 interface to Lapack library'
project_homepage='http://www.netlib.org/lapack95/'

project_headers=[]
project_source_files=[]
project_test_files = []
build_args = []
is_install = not (meson.is_subproject() and get_option('default_library') == 'static')

#===========================================
# Add sources
#===========================================

subdir('src')

#
# Dependency
#
lapack_dep = dependency('lapack')
deps = [lapack_dep]

#===========================================
# Compiler specific
#===========================================

if meson.get_compiler('fortran').get_id() == 'gnu'
  extra_args=[]
else
  extra_args=[]
endif


#============================================
# target
#============================================

public_headers = include_directories('include')
build_args +=[
  '-DPROJECT_NAME='+meson.project_name(),
  '-DPROJECT_VERSION='+meson.project_version(),
]

project_target = library(
  meson.project_name(),
  project_source_files,
  install: is_install,
  fortran_args: build_args,
  gnu_symbol_visibility: 'hidden',
  include_directories: public_headers,
  version: meson.project_version(),
  soversion: '0',
  dependencies: deps
)

project_inc = project_target.private_dir_include()

#=============================================
# project
#=============================================

# Make this library usable as a Meson subproject.
project_dep = declare_dependency(
  include_directories: project_inc,
  link_with : project_target
)
set_variable(meson.project_name() + '_dep', project_dep)

#======================================================
# License
#======================================================

# Package the license files
project_lic = files(
  'LICENSE-GPL-3',
)

#======================================================
# Install the package
#======================================================

if is_install
  # Distribute the license files in share/licenses/<name>
  install_data(
    project_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )

  # Make this library usable from the system's
  # package manager.
  # install_headers(project_headers, subdir : meson.project_name())
  module_id = meson.project_name() / 'modules'
  meson.add_install_script(
    find_program(files('config'/'install-mod.py')),
    get_option('includedir') / module_id,
  )

  pkg_mod = import('pkgconfig')
  pkg_mod.generate(
    name : meson.project_name(),
    filebase : meson.project_name(),
    description : project_description,
    libraries : project_target,
    subdirs: ['', module_id],
  )

endif


# ==============================================
# Unit Tests
# ==============================================

# if not meson.is_subproject()
#   add_languages('cpp')
#   subdir('tests')

#   test('all_tests',
#     executable(
#       'run_tests',
#       files(project_test_files),
#       dependencies : [project_dep, test_dep],
#       install : false
#     )
#   )
# endif