! MIT License

! Copyright (c) 2023 0382 and argparse-f's contributors

! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:

! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.

! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module argparse
  implicit none
  private
  integer, parameter :: type_string_len = 16
  integer, parameter :: short_name_len = 2
  integer, parameter :: long_name_len = 32
  integer, parameter :: argument_len = 32
  integer, parameter :: help_len = 1024
  integer, parameter :: value_len = 1024

  integer, parameter :: description_len = 1024
  integer, parameter :: program_name_len = 1024
  integer, parameter :: default_index_value = -1

  abstract interface
    subroutine sc_option_callback
    end subroutine sc_option_callback
  end interface
  !> Hack for Intel Fortran
  procedure(sc_option_callback), pointer :: dummy_print_help_wrapper => dummy_print_help

  type short_circuit_option
    character(len=short_name_len) :: short_name
    character(len=long_name_len) :: long_name
    character(len=help_len) :: help
    procedure(sc_option_callback), pointer, nopass :: callback
  end type short_circuit_option

  type option
    character(len=short_name_len) :: short_name
    character(len=long_name_len) :: long_name
    character(len=help_len) :: help
    character(len=type_string_len) :: value_type
    character(len=value_len) :: value
  end type option

  type argument
    character(len=argument_len) :: name
    character(len=help_len) :: help
    character(len=type_string_len) :: value_type
    character(len=value_len) :: value
  end type argument

  public :: argparser
  type :: argparser
    character(len=description_len) :: description
    character(len=program_name_len) :: program_name
    integer :: sc_option_size ! 手动管理一个类似 c++ std::vector 的动态数组
    type(short_circuit_option), dimension(:), allocatable :: sc_options
    integer :: option_size
    type(option), dimension(:), allocatable :: options
    integer :: named_argument_size
    type(argument), dimension(:), allocatable :: named_arguments
    integer :: argument_size
    type(argument), dimension(:), allocatable :: arguments
    integer, dimension(0:127) :: short_name_index = default_index_value
  contains
    final :: deallocate_argparser
    procedure :: parse => argp_parse
    procedure :: print_usage => argp_print_usage
    procedure :: print_help => argp_print_help
    procedure :: set_program_name => argp_set_program_name
    procedure :: add_sc_option => argp_add_sc_option
    procedure :: add_help_option => argp_add_help_option
    procedure :: add_option_logical => argp_add_option_logical
    procedure :: add_option_integer => argp_add_option_integer
    procedure :: add_option_real => argp_add_option_real
    procedure :: add_option_double => argp_add_option_double
    procedure :: add_option_string => argp_add_option_string
    procedure :: add_named_argument_integer => argp_add_named_argument_integer
    procedure :: add_named_argument_real => argp_add_named_argument_real
    procedure :: add_named_argument_double => argp_add_named_argument_double
    procedure :: add_named_argument_string => argp_add_named_argument_string
    procedure :: add_argument_integer => argp_add_argument_integer
    procedure :: add_argument_real => argp_add_argument_real
    procedure :: add_argument_double => argp_add_argument_double
    procedure :: add_argument_string => argp_add_argument_string
    procedure :: print_as_ini => argp_print_as_ini
    procedure :: has_option => argp_has_option
    procedure :: get_option_logical => argp_get_option_logical
    procedure :: get_option_integer => argp_get_option_integer
    procedure :: get_option_real => argp_get_option_real
    procedure :: get_option_double => argp_get_option_double
    procedure :: get_option_string => argp_get_option_string
    procedure :: get_argument_integer => argp_get_argument_integer
    procedure :: get_argument_real => argp_get_argument_real
    procedure :: get_argument_double => argp_get_argument_double
    procedure :: get_argument_string => argp_get_argument_string
  end type argparser

  interface argparser
    procedure :: make_argparser
  end interface

contains

  pure function make_argparser(description) result(this)
    character(len=*), intent(in) :: description
    type(argparser) :: this
    this%description = description
    this%program_name = ''
    this%sc_option_size = 0
    this%option_size = 0
    this%named_argument_size = 0
    this%argument_size = 0
    ! 主要是为了后面不用判断是否 `allocated`
    allocate (this%sc_options(1))
    allocate (this%options(1))
    allocate (this%named_arguments(1))
    allocate (this%arguments(1))
  end function make_argparser

  pure subroutine deallocate_argparser(this)
    type(argparser), intent(inout) :: this
    if (allocated(this%sc_options)) deallocate (this%sc_options)
    if (allocated(this%options)) deallocate (this%options)
    if (allocated(this%named_arguments)) deallocate (this%named_arguments)
    if (allocated(this%arguments)) deallocate (this%arguments)
  end subroutine

  subroutine argp_parse(this)
    class(argparser), intent(inout) :: this
    integer :: i, j, idx, argc, status
    character(len=value_len), dimension(:), allocatable :: tokens
    character(len=value_len) :: tok
    integer :: token_parsed_num
    argc = command_argument_count()
    allocate (tokens(argc))
    token_parsed_num = 0
    ! if not set program name, use argv[0]
    if (this%program_name == "") then
      call get_command_argument(0, this%program_name, status=status)
      if (status == -1) then
        print "(A)", "WARNING: you get a truncated program name"
      end if
    end if
    if (argc == 0 .and. (this%argument_size /= 0 .or. this%named_argument_size /= 0)) then
      call this%print_usage()
      stop
    end if
    do i = 1, argc
      call get_command_argument(i, tokens(i), status)
      if (status == -1) then
        print '(A)', "WARNING: the command argument, '"//trim(tokens(i))//"' is truncated, you'd better limit it in 1024 characters"
      end if
    end do
    ! parse short circuit options
    do i = 1, this%sc_option_size
      do j = 1, argc
        tok = tokens(j)
        if (tok == this%sc_options(i)%short_name .or. tok == this%sc_options(i)%long_name) then
          if (associated(this%sc_options(i)%callback, dummy_print_help_wrapper)) then
            call this%print_help()
          else
            call this%sc_options(i)%callback()
          end if
          stop
        end if
      end do
    end do
    ! parse options
    do i = 1, this%option_size
      token_parsed_num = 0
      do j = 1, argc
        tok = tokens(j)
        if (tok == this%options(i)%short_name .or. tok == this%options(i)%long_name) then
          if (this%options(i)%value_type == "logical") then
            this%options(i)%value = 'T'
            token_parsed_num = 1
            exit
          else
            if (j == argc) then
              error stop "(parse error) option '"//trim(this%options(i)%long_name)//"' should have value"
            end if
            this%options(i)%value = tokens(j + 1)
            token_parsed_num = 2
            exit
          end if
        end if
      end do
      if (token_parsed_num /= 0) then
        tokens(j:argc - token_parsed_num) = tokens(j + token_parsed_num:argc)
        argc = argc - token_parsed_num
      end if
    end do
    ! try parse aggregate short name options
    j = 1
    do while (j <= argc)
      tok = tokens(j)
      j = j + 1
      if (tok(1:1) /= '-') cycle
      if (len_trim(tok) <= 2) cycle
      tok(1:value_len - 1) = tok(2:value_len)
      do i = 1, len_trim(tok)
        idx = this%short_name_index(ichar(tok(i:i)))
        ! short circuit option
        if (idx <= this%sc_option_size .and. this%sc_options(idx)%short_name(2:2) == tok(i:i)) then
          if (associated(this%sc_options(idx)%callback, dummy_print_help_wrapper)) then
            call this%print_help()
          else
            call this%sc_options(idx)%callback()
          end if
          stop
        end if
        ! normal option
        if (idx <= this%option_size .and. this%options(idx)%short_name(2:2) == tok(i:i)) then
          if (this%options(idx)%value_type == "logical") then
            this%options(idx)%value = 'T'
          else
            error stop "(parse error) aggregate short name options must be logical"
          end if
        else
          error stop "(parse error) unrecognized short name option '"//tok(i:i)//"' in -"//trim(tok)
        end if
      end do
      tokens(j - 1:argc - 1) = tokens(j:argc)
      argc = argc - 1
    end do
    ! parse named argument
    if (argc < this%named_argument_size) then
      error stop "(parse error) not enough named_arguments"
    end if
    do i = 1, this%named_argument_size
      token_parsed_num = 0
      do j = 1, argc
        tok = tokens(j)
        if (try_parse_named_argument(tok, this%named_arguments(i))) then
          token_parsed_num = 1
          exit
        end if
      end do
      if (token_parsed_num /= 0) then
        tokens(j:argc - token_parsed_num) = tokens(j + token_parsed_num:argc)
        argc = argc - token_parsed_num
      end if
      if (this%named_arguments(i)%value == "") then
        error stop "(parse error) named_argument "//this%named_arguments(i)%name//" should have value"
      end if
    end do
    ! start parse position argument
    if (argc /= this%argument_size) then
      print '(A,I0,A,I0)', "(parse error) position argument number missmatching, give ", argc, ", but need ", this%argument_size
      error stop
    end if
    do i = 1, this%argument_size
      this%arguments(i)%value = tokens(i)
    end do
    deallocate (tokens)
  end subroutine argp_parse

  logical function try_parse_named_argument(line, arg) result(ans)
    character(len=*), intent(in) :: line
    type(argument), intent(inout) :: arg
    character(len=argument_len) :: name
    integer :: i, line_size
    line_size = len_trim(line)
    do i = 1, line_size
      if (line(i:i) == '=') exit
    end do
    if (i == line_size .and. line(i:i) /= '=') then
      ans = .false.
    else
      name = line(1:i - 1)
      if (name /= arg%name) then
        ans = .false.
      else
        arg%value = line(i + 1:line_size)
        ans = .true.
      end if
    end if
  end function try_parse_named_argument

  subroutine argp_set_program_name(this, program_name)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: program_name
    if (len_trim(program_name) > program_name_len) then
      print '(A,A,A)', "WARNING: program name: '", program_name, "' is too long"
    end if
    this%program_name = program_name
  end subroutine argp_set_program_name

  subroutine argp_print_usage(this)
    class(argparser), intent(in) :: this
    integer :: i
    write (*, '("usage: ",A," [options]")', advance='no') trim(this%program_name)
    do i = 1, this%named_argument_size
      write (*, '(" [=",A,"]")', advance='no') trim(this%named_arguments(i)%name)
    end do
    do i = 1, this%argument_size
      write (*, '(" [",A,"]")',  advance='no') trim(this%arguments(i)%name)
    end do
    print *, ""
  end subroutine argp_print_usage

  subroutine argp_print_help(this)
    class(argparser), intent(in) :: this
    integer :: i, j, length, max_name_length, printed_length
    character(len=32) :: help_fmt
    character(len=help_len), dimension(:), allocatable :: help_split

    call this%print_usage()
    print *, ""
    call split(this%description, "\n", help_split)
    do i = 1, size(help_split)
      print '(A)', trim(help_split(i))
    end do
    deallocate(help_split)
    print '(/,A)', "options:"

    ! calculate the longest option name
    max_name_length = 0
    do i = 1, this%sc_option_size
      length = len_trim(this%sc_options(i)%long_name)
      if (this%sc_options(i)%short_name /= "") then
        length = length + 4
      end if
      max_name_length = max(length, max_name_length)
    end do
    do i = 1, this%option_size
      length = len_trim(this%options(i)%long_name)
      if (this%options(i)%short_name /= "") then
        length = length + 4
      end if
      max_name_length = max(length, max_name_length)
    end do
    max_name_length = max(max_name_length, 25)

    ! print options
    do i = 1, this%sc_option_size
      write (*, '(A2)', advance='no') "  "
      printed_length = 0
      if (this%sc_options(i)%short_name /= "") then
        write (*, '(A,", ")', advance='no') trim(this%sc_options(i)%short_name)
        printed_length = 4
      end if
      write (*, '(A)', advance='no') trim(this%sc_options(i)%long_name)
      printed_length = printed_length + len_trim(this%sc_options(i)%long_name)
      write (unit=help_fmt, fmt='("(",I0,"X,A)")') max_name_length - printed_length
      write (*, help_fmt, advance='no') ''
      call split(this%sc_options(i)%help, "\n", help_split)
      print '(A)', trim(help_split(1))
      write (unit=help_fmt, fmt='("(",I0,"X,A)")') max_name_length + 2
      do j = 2, size(help_split, 1)
        print help_fmt, trim(help_split(j))
      end do
      deallocate (help_split)
    end do
    do i = 1, this%option_size
      write (*, '(A2)', advance='no') "  "
      printed_length = 0
      if (this%options(i)%short_name /= "") then
        write (*, '(A,", ")', advance='no') trim(this%options(i)%short_name)
        printed_length = 4
      end if
      write (*, '(A)', advance='no') trim(this%options(i)%long_name)
      printed_length = printed_length + len_trim(this%options(i)%long_name)
      write (unit=help_fmt, fmt='("(",I0,"X,A)")') max_name_length - printed_length
      write (*, help_fmt, advance='no') ''
      call split(this%options(i)%help, "\n", help_split)
      if (this%options(i)%value_type == "logical") then
        print '(A)', trim(help_split(1))
      else
        print '("(",A," [=",A,"]) ",A)', trim(this%options(i)%value_type), trim(this%options(i)%value), trim(help_split(1))
      end if
      write (unit=help_fmt, fmt='("(",I0,"X,A)")') max_name_length + 2
      do j = 2, size(help_split, 1)
        print help_fmt, trim(help_split(j))
      end do
      deallocate (help_split)
    end do

    if (this%named_argument_size > 0) then
      print '(/,A)', "named arguments:"
      max_name_length = 0
      do i = 1, this%named_argument_size
        max_name_length = max(max_name_length, len_trim(this%named_arguments(i)%name))
      end do
      max_name_length = max(max_name_length, 25)
      do i = 1, this%named_argument_size
        write (*, '(2X,A)', advance='no') trim(this%named_arguments(i)%name)
        printed_length = len_trim(this%named_arguments(i)%name)
        write (unit=help_fmt, fmt='("(",I0,"X,""("",A,"") "")")') max_name_length - printed_length
        ! print '(A)', help_fmt
        write (*, help_fmt, advance='no') trim(this%named_arguments(i)%value_type)
        call split(this%named_arguments(i)%help, "\n", help_split)
        print '(A)', trim(help_split(1))
        write (unit=help_fmt, fmt='("(",I0,"X,A)")') max_name_length + 2
        do j = 2, size(help_split, 1)
          print help_fmt, trim(help_split(j))
        end do
        deallocate (help_split)
      end do
    end if

    if (this%argument_size > 0) then
      print '(/,A)', "Position rguments:"
      max_name_length = 0
      do i = 1, this%argument_size
        max_name_length = max(max_name_length, len_trim(this%arguments(i)%name))
      end do
      max_name_length = max(max_name_length, 25)
      do i = 1, this%argument_size
        write (*, '(2X,A)', advance='no') trim(this%arguments(i)%name)
        printed_length = len_trim(this%arguments(i)%name)
        write (unit=help_fmt, fmt='("(",I0,"X,""("",A,"") "")")') max_name_length - printed_length
        write (*, help_fmt, advance='no') trim(this%arguments(i)%value_type)
        call split(this%arguments(i)%help, "\n", help_split)
        print '(A)', trim(help_split(1))
        write (unit=help_fmt, fmt='("(",I0,"X,A)")') max_name_length + 2
        do j = 2, size(help_split, 1)
          print help_fmt, trim(help_split(j))
        end do
        deallocate (help_split)
      end do
    end if
  end subroutine argp_print_help

  subroutine argp_print_as_ini(this, unit, comment)
    use iso_fortran_env
    class(argparser), intent(in) :: this
    integer, optional, intent(in) :: unit
    logical, optional, intent(in) :: comment
    integer :: print_unit, i, j, str_len
    logical :: print_comment
    character(len=8) :: logical_str
    character(len=help_len), dimension(:), allocatable :: help_split
    print_unit = output_unit
    if (present(unit)) then
      print_unit = unit
    end if
    print_comment = .false.
    if (present(comment)) then
      print_comment = comment
    end if
    if (this%option_size > 0) then
      write (unit=print_unit, fmt='(A)') "[options]"
    end if
    do i = 1, this%option_size
      if (print_comment) then
        call split(this%options(i)%help, "\n", help_split)
        do j = 1, size(help_split, 1)
          write (unit=print_unit, fmt='("# ",A)') trim(help_split(j))
        end do
        deallocate (help_split)
      end if
      str_len = len_trim(this%options(i)%long_name)
      if (this%options(i)%value_type == "logical") then
        ! for common INI file
        if (this%options(i)%value == 'T') then
          logical_str = "true"
        else
          logical_str = "false"
        end if
        write (unit=print_unit, fmt='(A," = ",A)') this%options(i)%long_name(3:str_len), trim(logical_str)
      else
        write (unit=print_unit, fmt='(A," = ",A)') this%options(i)%long_name(3:str_len), trim(this%options(i)%value)
      end if
    end do
    if (this%named_argument_size > 0) then
      write (unit=print_unit, fmt='(A)') "[named_arguments]"
    end if
    do i = 1, this%named_argument_size
      if (print_comment) then
        call split(this%named_arguments(i)%help, "\n", help_split)
        do j = 1, size(help_split, 1)
          write (unit=print_unit, fmt='("# ",A)') trim(help_split(j))
        end do
        deallocate (help_split)
      end if
      write (unit=print_unit, fmt='(A," = ",A)') trim(this%named_arguments(i)%name), trim(this%named_arguments(i)%value)
    end do
    if (this%argument_size > 0) then
      write (unit=print_unit, fmt='(A)') "[arguments]"
    end if
    do i = 1, this%argument_size
      if (print_comment) then
        call split(this%arguments(i)%help, "\n", help_split)
        do j = 1, size(help_split, 1)
          write (unit=print_unit, fmt='("# ",A)') trim(help_split(j))
        end do
        deallocate (help_split)
      end if
      write (unit=print_unit, fmt='(A," = ",A)') trim(this%arguments(i)%name), trim(this%arguments(i)%value)
    end do
  end subroutine

  subroutine argp_add_sc_option(this, short_name, long_name, help, callback)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    procedure(sc_option_callback) :: callback
    integer :: t_sc_size, idx
    type(short_circuit_option), dimension(:), allocatable :: t_sc_opts
    ! long name must not be empty
    call argp_check_long_name(this, long_name)
    ! allow short name to be empty
    if (short_name /= "") then
      call argp_check_short_name(this, short_name)
      idx = ichar(short_name(2:2))
      this%short_name_index(idx) = this%sc_option_size + 1
    end if
    ! 手动管理变长数组
    t_sc_size = size(this%sc_options, 1)
    if (t_sc_size == this%sc_option_size) then
      allocate (t_sc_opts(t_sc_size))
      t_sc_opts(1:t_sc_size) = this%sc_options
      deallocate (this%sc_options)
      allocate (this%sc_options(2*t_sc_size))
      this%sc_options(1:t_sc_size) = t_sc_opts
      deallocate (t_sc_opts)
    end if
    this%sc_option_size = this%sc_option_size + 1
    idx = this%sc_option_size
    this%sc_options(idx)%short_name = short_name
    this%sc_options(idx)%long_name = long_name
    this%sc_options(idx)%help = help
    this%sc_options(idx)%callback => callback
  end subroutine argp_add_sc_option

  subroutine dummy_print_help()
  end subroutine dummy_print_help

  subroutine argp_add_help_option(this)
    class(argparser), intent(inout) :: this
    type(argparser), save :: temp
    temp = this
    call this%add_sc_option("-?", "--help", "show this help message", dummy_print_help)
    !! this cannot work, use `dummp_print_help` to compromise
    ! contains
    !   subroutine local_print_help
    !     call temp%print_help()
    !   end subroutine local_print_help
  end subroutine argp_add_help_option

  pure subroutine argp_try_add_option(this, short_name, long_name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    integer :: t_opt_size, idx
    type(option), dimension(:), allocatable :: t_opts
    call argp_check_long_name(this, long_name)
    if (short_name /= "") then
      call argp_check_short_name(this, short_name)
      idx = ichar(short_name(2:2))
      this%short_name_index(idx) = this%option_size + 1
    end if
    ! 手动管理变长数组
    t_opt_size = size(this%options, 1)
    if (t_opt_size == this%option_size) then
      allocate (t_opts(t_opt_size))
      t_opts(1:t_opt_size) = this%options
      deallocate (this%options)
      allocate (this%options(2*t_opt_size))
      this%options(1:t_opt_size) = t_opts
      deallocate (t_opts)
    end if
    this%option_size = this%option_size + 1
    idx = this%option_size
    this%options(idx)%short_name = short_name
    this%options(idx)%long_name = long_name
    this%options(idx)%help = help
  end subroutine argp_try_add_option

  pure subroutine argp_add_option_logical(this, short_name, long_name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    integer :: idx
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "logical"
    this%options(idx)%value = "F"
  end subroutine argp_add_option_logical

  pure subroutine argp_add_option_integer(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    integer, intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "integer"
    write (unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_integer

  pure subroutine argp_add_option_real(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    real, intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "real"
    write (unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_real

  pure subroutine argp_add_option_double(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    real(kind=8), intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "double"
    write (unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_double

  pure subroutine argp_add_option_string(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    character(len=*), intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "string"
    write (unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_string

  pure subroutine argp_try_add_argument(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: t_arg_size, idx
    type(argument), dimension(:), allocatable :: t_args
    call argp_check_argument_name(this, name)
    ! 手动管理变长数组
    t_arg_size = size(this%arguments, 1)
    if (t_arg_size == this%argument_size) then
      allocate (t_args(t_arg_size))
      t_args(:) = this%arguments
      deallocate (this%arguments)
      allocate (this%arguments(2*t_arg_size))
      this%arguments(1:t_arg_size) = t_args
      deallocate (t_args)
    end if
    this%argument_size = this%argument_size + 1
    idx = this%argument_size
    this%arguments(idx)%name = name
    this%arguments(idx)%help = help
  end subroutine argp_try_add_argument

  pure subroutine argp_try_add_named_argument(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: t_arg_size, idx
    type(argument), dimension(:), allocatable :: t_args
    call argp_check_argument_name(this, name)
    ! 手动管理变长数组
    t_arg_size = size(this%named_arguments, 1)
    if (t_arg_size == this%named_argument_size) then
      allocate (t_args(t_arg_size))
      t_args(:) = this%named_arguments
      deallocate (this%named_arguments)
      allocate (this%named_arguments(2*t_arg_size))
      this%named_arguments(1:t_arg_size) = t_args
      deallocate (t_args)
    end if
    this%named_argument_size = this%named_argument_size + 1
    idx = this%named_argument_size
    this%named_arguments(idx)%name = name
    this%named_arguments(idx)%help = help
  end subroutine argp_try_add_named_argument

  pure subroutine argp_add_argument_integer(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "integer"
  end subroutine argp_add_argument_integer

  pure subroutine argp_add_argument_real(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "real"
  end subroutine argp_add_argument_real

  pure subroutine argp_add_argument_double(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "double"
  end subroutine argp_add_argument_double

  pure subroutine argp_add_argument_string(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "string"
  end subroutine argp_add_argument_string

  pure subroutine argp_add_named_argument_integer(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "integer"
  end subroutine argp_add_named_argument_integer

  pure subroutine argp_add_named_argument_real(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "real"
  end subroutine argp_add_named_argument_real

  pure subroutine argp_add_named_argument_double(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "double"
  end subroutine argp_add_named_argument_double

  pure subroutine argp_add_named_argument_string(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "string"
  end subroutine argp_add_named_argument_string

  pure integer function argp_find_option(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    do i = 1, this%option_size
      if (name == this%options(i)%short_name .or. name == this%options(i)%long_name) then
        ans = i
        return
      end if
    end do
    error stop "(get error) option not found: "//trim(name)
  end function argp_find_option

  pure subroutine argp_check_option_type(this, idx, type)
    class(argparser), intent(in) :: this
    integer, intent(in) :: idx
    character(len=*), intent(in) :: type
    if (this%options(idx)%value_type /= type) then
      error stop "(get error) option '"//trim(this%options(idx)%long_name)//"' is set as " &
        //trim(this%options(idx)%value_type)//", you try to get as "//trim(type)
    end if
  end subroutine argp_check_option_type

  pure logical function argp_get_option_logical(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    call argp_check_option_type(this, i, "logical")
    read (unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_logical

  pure logical function argp_has_option(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    ans = argp_get_option_logical(this, name)
  end function argp_has_option

  pure integer function argp_get_option_integer(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    call argp_check_option_type(this, i, "integer")
    read (unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_integer

  pure real function argp_get_option_real(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    call argp_check_option_type(this, i, "real")
    read (unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_real

  pure real(kind=8) function argp_get_option_double(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    call argp_check_option_type(this, i, "double")
    read (unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_double

  pure function argp_get_option_string(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    character(len=value_len) :: ans
    integer :: i
    i = argp_find_option(this, name)
    call argp_check_option_type(this, i, "string")
    ans = this%options(i)%value
  end function argp_get_option_string

  pure integer function argp_find_argument(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    do i = 1, this%named_argument_size
      if (name == this%named_arguments(i)%name) then
        ans = i
        return
      end if
    end do
    do i = 1, this%argument_size
      if (name == this%arguments(i)%name) then
        ans = -i
        return
      end if
    end do
    error stop "(get error) argument not found: "//trim(name)
  end function argp_find_argument

  pure subroutine argp_check_argument_type(this, idx, type)
    class(argparser), intent(in) :: this
    integer, intent(in) :: idx
    character(len=*), intent(in) :: type
    character(len=type_string_len) :: arg_type
    character(len=argument_len) :: name
    if (idx > 0) then
      arg_type = this%named_arguments(idx)%value_type
      name = this%named_arguments(idx)%name
    else
      arg_type = this%arguments(-idx)%value_type
      name = this%arguments(-idx)%name
    end if
    if (arg_type /= type) then
      error stop "(get error) argument '"//trim(name)//"' is set as "//trim(arg_type)//", you try to get as "//trim(type)
    end if
  end subroutine argp_check_argument_type

  pure integer function argp_get_argument_integer(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_argument(this, name)
    call argp_check_argument_type(this, i, "integer")
    if (i > 0) then
      read (unit=this%named_arguments(i)%value, fmt=*) ans
    else
      read (unit=this%arguments(-i)%value, fmt=*) ans
    end if
  end function argp_get_argument_integer

  pure real function argp_get_argument_real(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_argument(this, name)
    call argp_check_argument_type(this, i, "real")
    if (i > 0) then
      read (unit=this%named_arguments(i)%value, fmt=*) ans
    else
      read (unit=this%arguments(-i)%value, fmt=*) ans
    end if
  end function argp_get_argument_real

  pure real(kind=8) function argp_get_argument_double(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_argument(this, name)
    call argp_check_argument_type(this, i, "double")
    if (i > 0) then
      read (unit=this%named_arguments(i)%value, fmt=*) ans
    else
      read (unit=this%arguments(-i)%value, fmt=*) ans
    end if
  end function argp_get_argument_double

  pure function argp_get_argument_string(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    character(len=value_len) :: ans
    integer :: i
    i = argp_find_argument(this, name)
    call argp_check_argument_type(this, i, "string")
    if (i > 0) then
      ans = this%named_arguments(i)%value
    else
      ans = this%arguments(-i)%value
    end if
  end function argp_get_argument_string

  pure subroutine argp_check_short_name(this, name)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: name_size, char_pos
    name_size = len(name)
    if (name_size /= 2 .or. name(1:1) /= '-') then
      error stop "(build error) short option name must be `-` followed by single character"
    end if
    char_pos = ichar(name(2:2))
    if (this%short_name_index(char_pos) /= default_index_value) then
      error stop "(build error) short option name "//trim(name)//" already exists"
    end if
  end subroutine argp_check_short_name

  pure subroutine argp_check_long_name(this, name)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    if (name == "") then
      error stop "(build error) long option name cannot be empty"
    end if
    if (name(1:2) /= "--") then
      error stop "(build error) long option name must starts with `--`"
    end if
    do i = 1, this%sc_option_size
      if (name == trim(this%sc_options(i)%long_name)) then
        error stop "(build error) long option name "//trim(name)//" already exists"
      end if
    end do
    do i = 1, this%option_size
      if (name == trim(this%options(i)%long_name)) then
        error stop "(build error) long option name "//trim(name)//" already exists"
      end if
    end do
  end subroutine argp_check_long_name

  pure subroutine argp_check_argument_name(this, name)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    if (name == "") then
      error stop "(build error) argument name cannot be empty"
    end if
    do i = 1, this%argument_size
      if (name == trim(this%arguments(i)%name)) then
        error stop "(build error) argument name "//trim(name)//" already exists"
      end if
    end do
    do i = 1, this%named_argument_size
      if (name == trim(this%named_arguments(i)%name)) then
        error stop "(build error) argument name "//trim(name)//" already exists"
      end if
    end do
  end subroutine argp_check_argument_name

  pure subroutine split(line, sep, result)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: sep
    character(len=*), dimension(:), allocatable, intent(out) :: result
    integer :: i, di, start, line_size, sep_size, res_count
    line_size = len_trim(line)
    sep_size = len_trim(sep)
    res_count = 1
    i = index(line(1:line_size), sep)
    di = i
    do while (di /= 0)
      res_count = res_count + 1
      di = index(line(i + sep_size:line_size), sep)
      i = i + sep_size + di - 1
    end do
    allocate (result(res_count))
    res_count = 1
    i = index(line(1:line_size), sep)
    di = i
    start = 1
    do while (di /= 0)
      result(res_count) = line(start:i - 1)
      res_count = res_count + 1
      start = i + sep_size
      di = index(line(start:line_size), sep)
      i = start + di - 1
    end do
    result(res_count) = line(start:line_size)
  end subroutine
end module argparse
