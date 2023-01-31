# argparse-f

Modern Fortran command line parser, implemented with OOP.

## Example
```fortran
program test
    use iso_fortran_env, only : stdout=>output_unit
    use argparse
    implicit none
    character(len=10), parameter :: program_name = "qcalc"
    type(argparser) :: args
    args = argparser("A quantum physics calculation program.")
    call args%set_program_name(program_name)
    call args%add_help_option()
    call args%add_sc_option("-v", "--version", "show version info", show_version_info)
    call args%add_option_logical("-o", "--openmp", "use openmp or not") ! logical option has no default
    call args%add_option_logical("-m", "--mpi", "use mpi or not")
    call args%add_option_integer("-t", "--thread", "thread number, valid if openmp is set", 1)
    call args%add_option_integer("-p", "--process", "process number, valid if mpi is set", 1)
    call args%add_option_string("", "--chemical", "chemical formula", "H2O") ! short name can be empty
    call args%add_named_argument_string("input", "initialize file")
    call args%add_named_argument_string("output", "output file")
    call args%parse()

    if(args%has_option("--openmp")) then
        print '(A,I2,A)', "openmp is used, and we use ", args%get_option_integer("-t"), " threads"
    end if
    if(args%has_option("--mpi")) then
        print '(A,I2,A)', "mpi is used, and we use ", args%get_option_integer("-p"), " processes"
    end if
    print '(A,A)', "the calculated chemical is ", trim(args%get_option_string("--chemical"))
    print '(A,A)', "the input file is ", trim(args%get_argument_string("input"))
    print '(A,A)', "the output file is ", trim(args%get_argument_string("output"))

    ! you can also print the cached `args` into INI file
    ! print '(/,A)', "All of the options and arguments are shown below"
    ! call args%print_as_ini(stdout, .true.)
  contains
    subroutine show_version_info
        print "(A)", trim(program_name)//" version 0.1.0"
    end subroutine show_version_info
end program test
```
Compile it as `qclac`, and use it like:
```bash
> qclac -?
usage: qcalc [options] [=input] [=output] 

A quantum physics calculation program.

Options:
  -?, --help              show this help message
  -v, --version           show version info
  -o, --openmp            use openmp or not
  -m, --mpi               use mpi or not
  -t, --thread            (integer) thread number, valid if openmp is set
  -p, --process           (integer) process number, valid if mpi is set
      --chemical          (string) chemical formula

Named arguments:
  input                   (string) initialize file
  output                  (string) output file
> qclac -o -t 4 input=input.txt output=out.bin
openmp is used, and we use  4 threads
the calculated chemical is H2O
the input file is input.txt
the output file is out.bin
```

## Usage

The simple way to use this package is copy the file `src/argparse-f.f90` into your project. Or you can use fpm
```toml
[dependencies]
argparse-f = { git="https://github.com/0382/argparse-f.git" }
```

## Parse rules

In this package, command line arguments are classified into two kinds: `option` and `argument`.

### `option`

As the name suggests, `option` is optional. It cantains two types: normal option and short curcuit option.

#### normal options

You can add normal options like this
```fortran
call args%add_option_integer("-t", "--thread", "thread number", 1)
```

The first dummy argument means `short_name` of the option, and it can be empty. If it is not empty, then it must be `-` followed by **single character**. The second dummy argument means `long_name` of the option, and it **cannot** be empty, it must start with `--`. The third dummy argument means help message, and the last one is the default value of the option.

In this version, normal option support five data types: `logical, integer, real, real(kind=8), character(len=*)` (add `real(8)` option method is `add_option_double`, and add `character(len=*)` option method is `add_option_string`).

`add_option_logical` function does not need the default value. Because, if you set the option in command line, the value is `.true.`, otherwise the value is `.false.`, for example:
```bash
ls -l
```
them `-l` option is set to `.true.`. To add the other four data type's options, you must give the default value, in case if one do not set the option, we use the default value. In command line, you should set the option like this:
```bash
greet --name Alice --age 24
```
then the `--name` option is set to `Alice` and `--age` option is set to `24`.

#### short circuit options

You can add short circuit options like this:
```fortran
call args%add_sc_option("-v", "--version", "show version info", show_version_info)
contains
subroutine show_version_info
    print "(A)", trim(program_name)//" version 0.1.0"
end subroutine show_version_info
```

A short circuit option must be `logical` type, and you should give a callback subroutine. The callback subroutine cannot have dummy arguments. Short circuit options are searched first, for example
```bash
git --help
gcc -v
```
The corresponding callback subroutine is called immediately as long as the program searched the first short circuit option, and then the program `stop`.

### argument

`argument` is opposited to `option`. You must set it's value in command line. If not, the program will stop with error. `argument` also contains two types: position argument and named argument.

In this version, `argument` supports `integer, real, real(kind=8), character(len=*)` data types. It does not support `logical` type, you should use `option` instead.
#### position argument

Position argument is got with position, for exmple
```bash
ffplay video.mp4
```
The `video.mp4` is the first (and only) position argument. If you do not give the argument, the `ffplay` will exit with error.

Add position argument like this:
```fortran
call args%add_argument_string("input", "initialize file")
```
The first dummy argument is `name` of the argumet, and the second is the help message.

#### named argument

The named argument is defined by myself, it is designed for my work. It is used like this:
```bash
greet name=Alice age=24
```
It is tedious comparing with position argument, so it should not used in a common command line program. But it useful in a big project, and you run program with a shell script. In this case, the named argument make the script more readable.

Add named argument like this:
```fortran
call args%add_named_argument_string("input", "initialize file")
```

## Get results

### option

Use functions like `args%get_option_logical(name)` to get option results. You can use both `short_name` and `long_name`. For `logical` data type, you can also use `args%has_option(name)` for short.

### argument

Use functions like `args%get_argument_logical(name)` to get argument results. Named argument and position argument cannot have duplicate name. So you can use this function to get either named argument or position argument.

## Tips

### custom help option

You can add default help option with `args%add_help_option`, it's `short_name` and `long_name` are `-?` and `--help`. If you dislike the names, you can add custom help option like this
```fortran
call args%add_sc_option("-h", "--help", "show this help message", print_help)
contains
subroutine print_help
    call args%print_help()
end subroutine
```

### print argparser

You can print the argparser into INI file format. If the second dummy argument is set to `.true.`, then print help message as comments.
```fortran
call args%print_as_ini(stdout, .true.)
```

## Reference

This package works like my c++ package [argparse](https://github.com/0382/util/tree/main/cpp/argparse). They are imspired by c++ package [cmdline](https://github.com/tanakh/cmdline) and python's standard library package [argparse](https://docs.python.org/3/library/argparse.html).