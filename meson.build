project(
    'argparse-f',
    'fortran',
    version : '0.1.0',
    license : 'MIT',
    default_options : [
        'buildtype=debugoptimized',
        'fortran_std=f2018'
    ]
)

argparse_f_src = files(
    'src/argparse-f.f90'
)

argparse_f_lib = static_library(
    meson.project_name(),
    sources : argparse_f_src,
    install : true,
)

argparse_f_inc = argparse_f_lib.private_dir_include()
argparse_f_dep = declare_dependency(
    link_with : argparse_f_lib,
    include_directories : argparse_f_inc,
)

test(
    'argparse_f_check',
    executable(
        'argparse_f_check',
        'test/check.f90',
        dependencies : argparse_f_dep,
    ),
)
