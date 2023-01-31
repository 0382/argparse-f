program check
    use iso_fortran_env, only: stdout => output_unit
    use argparse
    implicit none
    character(len=10), parameter :: program_name = "qcalc"
    type(argparser) :: args
    args = argparser("A quantum physics calculation program.")
    call args%set_program_name(program_name)
    call args%add_help_option()
    ! call args%add_sc_option("-h", "--help", "show this help message", print_help)
    call args%add_sc_option("-v", "--version", "show version info", show_version_info)
    call args%add_option_logical("-o", "--openmp", "use openmp or not") ! logical option has no default
    call args%add_option_logical("-m", "--mpi", "use mpi or not")
    call args%add_option_integer("-t", "--thread", "thread number,\nit is valid only if openmp is set", 1)
    call args%add_option_integer("-p", "--process", "process number,\nit is valid only if mpi is set", 1)
    call args%add_option_string("", "--chemical", "chemical formula", "H2O") ! short name can be empty
    call args%add_named_argument_string("input", "initialize file")
    call args%add_named_argument_string("output", "output file")
    call args%parse()

    if (args%has_option("--openmp")) then
        print '(A,I2,A)', "openmp is used, and we use ", args%get_option_integer("-t"), " threads"
    end if
    if (args%has_option("--mpi")) then
        print '(A,I2,A)', "mpi is used, and we use ", args%get_option_integer("-p"), " processes"
    end if
    print '(A,A)', "the calculated chemical is ", trim(args%get_option_string("--chemical"))
    print '(A,A)', "the input file is ", trim(args%get_argument_string("input"))
    print '(A,A)', "the output file is ", trim(args%get_argument_string("output"))

    ! you can also print the cached `args` into INI file
    print '(/,A)', "All of the options and arguments are shown below"
    call args%print_as_ini(stdout, .true.)
contains
    subroutine show_version_info
        print "(A)", trim(program_name)//" version 0.1.0"
    end subroutine show_version_info
    subroutine print_help
        call args%print_help()
    end subroutine
end program check
