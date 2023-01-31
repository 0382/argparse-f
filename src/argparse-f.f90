module argparse
  implicit none
  private
  integer, parameter :: type_string_len = 16
  integer, parameter :: short_name_len = 2
  integer, parameter :: long_name_len = 16
  integer, parameter :: argument_len = 16
  integer, parameter :: help_len = 1024
  integer, parameter :: value_len = 1024
  
  integer, parameter :: description_len = 1024
  integer, parameter :: program_name_len = 1024
  
  abstract interface
    subroutine sc_option_callback
    end subroutine sc_option_callback
  end interface
  
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
    integer, dimension(0:127) :: short_name_index
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

  function make_argparser(description) result(this)
    character(len=*) :: description
    type(argparser) :: this
    this%description = description
    this%program_name = ''
    this%sc_option_size = 0
    this%option_size = 0
    this%named_argument_size = 0
    this%argument_size = 0
    this%short_name_index(:) = -1
    ! 主要是为了后面不用判断是否 `allocated`
    allocate(this%sc_options(1))
    allocate(this%options(1))
    allocate(this%named_arguments(1))
    allocate(this%arguments(1))
  end function make_argparser

  subroutine deallocate_argparser(this)
    type(argparser), intent(inout) :: this
    if(allocated(this%sc_options)) deallocate(this%sc_options)
    if(allocated(this%options)) deallocate(this%options)
    if(allocated(this%named_arguments)) deallocate(this%named_arguments)
    if(allocated(this%arguments)) deallocate(this%arguments)
  end subroutine

  subroutine argp_parse(this)
    class(argparser), intent(inout) :: this
    integer :: i, j, argc, status
    character(len=value_len), dimension(:), allocatable :: tokens
    character(len=value_len) :: tok
    integer :: token_parsed_num
    argc = command_argument_count()
    allocate(tokens(argc))
    token_parsed_num = 0
    ! if not set program name, use argv[0]
    if(this%program_name == "") then
      call get_command_argument(0, this%program_name, status=status)
      if(status == -1) then
        print "(A)", "WARNING: you get a truncated program name"
      end if
    end if
    if(argc == 0) then
      call this%print_usage()
      stop
    end if
    do i = 1,argc
      call get_command_argument(i, tokens(i), status)
      if(status == -1) then
        print '(A,A,I2)', "WARNING: the command argument, '", tokens(i), "' is truncated, you'd better limit it in 1024 characters"
      end if
    end do
    ! parse short circuit options
    do i = 1,this%sc_option_size
      do j = 1,argc
        tok = tokens(j)
        if(tok == this%sc_options(i)%short_name .or. tok == this%sc_options(i)%long_name) then
          if(associated(this%sc_options(i)%callback, dummy_print_help)) then
            call this%print_help()
          else
            call this%sc_options(i)%callback()
          end if
          stop
        end if
      end do
    end do
    ! parse options
    do i = 1,this%option_size
      token_parsed_num = 0
      do j = 1,argc
        tok = tokens(j)
        if(tok == this%options(i)%short_name .or. tok == this%options(i)%long_name) then
          if(this%options(i)%value_type == "logical") then
            this%options(i)%value = 'T'
            token_parsed_num = 1
            exit
          else
            if(j == argc) then
              error stop "(parse error) option '"//trim(this%options(i)%long_name)//"' should have value"
            end if
            this%options(i)%value = tokens(j+1)
            token_parsed_num = 2
            exit
          end if
        end if
      end do
      if(token_parsed_num /= 0) then
        tokens(j:argc-token_parsed_num) = tokens(j+token_parsed_num:argc)
        argc = argc - token_parsed_num
      end if
    end do
    ! parse aggregation short name options
    ! parse named argument
    if(argc < this%named_argument_size) then
      error stop "(parse error) not enough named_arguments"
    end if
    do i = 1,this%named_argument_size
      token_parsed_num = 0
      do j = 1,argc
        tok = tokens(j)
        if(try_parse_named_argument(tok, this%named_arguments(i))) then
          token_parsed_num = 1
          exit
        end if
      end do
      if(token_parsed_num /= 0) then
        tokens(j:argc-token_parsed_num) = tokens(j+token_parsed_num:argc)
        argc = argc - token_parsed_num
      end if
      if(this%named_arguments(i)%value == "") then
        error stop "(parse error) named_argument "//this%named_arguments(i)%name//" should have value"
      end if
    end do
    ! start parse position argument
    if(argc /= this%argument_size) then
      print '(A,I3,A,I3)', "(parse error) position argument number missmatching, give ", argc, ", but need", this%argument_size
      error stop
    end if
    do i = 1,this%argument_size
      this%arguments(i)%value = tokens(i)
    end do
  end subroutine argp_parse

  logical function try_parse_named_argument(line, arg) result(ans)
    character(len=*), intent(in) :: line
    type(argument), intent(inout) :: arg
    character(len=argument_len) :: name
    integer :: i, line_size
    line_size = len_trim(line)
    do i = 1,line_size
      if(line(i:i) == '=') exit
    end do
    if(i == line_size .and. line(i:i) /= '=') then
      ans = .false.
    else
      name = line(1:i-1)
      if(name /=  arg%name) then
        ans = .false.
      else
        arg%value = line(i+1:line_size)
        ans = .true.
      end if
    end if
  end function try_parse_named_argument

  subroutine argp_set_program_name(this, program_name)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: program_name
    if(len_trim(program_name) > program_name_len) then
      print '(A,A,A)', "WARNING: program name: '",program_name,"' is too long"
    end if
    this%program_name = program_name
  end subroutine argp_set_program_name

  subroutine argp_print_usage(this)
    class(argparser), intent(in) :: this
    integer :: i
    print '("usage: ",A," [options]",$)', trim(this%program_name)
    do i = 1, this%named_argument_size
      print '(" [=",A,"]",$)', trim(this%named_arguments(i)%name)
    end do
    do i = 1, this%argument_size
      print '(" [",A,"]",$)', trim(this%arguments(i)%name)
    end do
    print *, ""
  end subroutine argp_print_usage

  subroutine argp_print_help(this)
    class(argparser), intent(in) :: this
    integer :: i
    character(len=32) :: help_fmt
    
    call this%print_usage()
    print '(/,A,/,/,"Options:")', trim(this%description)
    
    do i = 1, this%sc_option_size
      if(this%sc_options(i)%short_name /= "") then
        print '(2X,A,", ",$)', trim(this%sc_options(i)%short_name)
      else
        print '(A6,$)', ' '
      end if
      print '(A,$)', trim(this%sc_options(i)%long_name)
      write(unit=help_fmt, fmt='("(",I2,"X,A)")') 20 - len_trim(this%sc_options(i)%long_name)
      print help_fmt, trim(this%sc_options(i)%help)
    end do

    do i = 1, this%option_size
      if(this%options(i)%short_name /= "") then
        print '(2X,A,", ",$)', trim(this%options(i)%short_name)
      else
        print '(A6,$)', ' '
      end if
      print '(A,$)', trim(this%options(i)%long_name)
      if(this%options(i)%value_type == "logical") then
        write(unit=help_fmt, fmt='("(",I2,"X,A)")') 20 - len_trim(this%options(i)%long_name)
        print help_fmt, trim(this%options(i)%help)
      else
        write(unit=help_fmt, fmt='("(",I2,"X,",A,",A)")') 20 - len_trim(this%options(i)%long_name), '"(",A,") "'
        print help_fmt, trim(this%options(i)%value_type), trim(this%options(i)%help)
      end if
    end do

    if(this%named_argument_size > 0) then
      print '(/,A)', "Named arguments:"
      do i = 1,this%named_argument_size
        print '(2X,A,$)', trim(this%named_arguments(i)%name)
        write(unit=help_fmt, fmt='("(",I2,"X,",A,",A)")') 24 - len_trim(this%named_arguments(i)%name), '"(",A,") "'
        print help_fmt, trim(this%named_arguments(i)%value_type), trim(this%named_arguments(i)%help)
      end do
    end if

    if(this%argument_size > 0) then
      print '(/,A)', "Position rguments:"
      do i = 1,this%argument_size
        print '(2X,A,$)', trim(this%arguments(i)%name)
        write(unit=help_fmt, fmt='("(",I2,"X,",A,",A)")') 24 - len_trim(this%arguments(i)%name), '"(",A,") "'
        print help_fmt, trim(this%arguments(i)%value_type), trim(this%arguments(i)%help)
      end do
    end if
  end subroutine argp_print_help

  subroutine argp_print_as_ini(this, unit, comment)
    use iso_fortran_env
    class(argparser), intent(in) :: this
    integer, optional, intent(in) :: unit
    logical, optional, intent(in) :: comment
    integer :: print_unit, i, str_len
    logical :: print_comment
    character(len=8) :: logical_str
    print_unit = output_unit
    if(present(unit)) then
      print_unit = unit
    end if
    print_comment = .false.
    if(present(comment)) then
      print_comment = comment
    end if
    if (this%option_size > 0) then
      write(unit=print_unit, fmt='(A)') "[options]"
    end if
    do i = 1,this%option_size
      if(print_comment) then
        write(unit=print_unit, fmt='(A,A)') "# ", trim(this%options(i)%help)
      end if
      str_len = len_trim(this%options(i)%long_name)
      if(this%options(i)%value_type == "logical") then
        ! for common INI file
        if(this%options(i)%value == 'T') then
          logical_str = "true"
        else
          logical_str = "false"
        end if
        write(unit=print_unit, fmt='(A,"=",A)') this%options(i)%long_name(3:str_len), trim(logical_str)
      else
        write(unit=print_unit, fmt='(A,"=",A)') this%options(i)%long_name(3:str_len), trim(this%options(i)%value)
      end if
    end do
    if(this%named_argument_size > 0) then
      write(unit=print_unit, fmt='(A)') "[named_arguments]"
    end if
    do i = 1,this%named_argument_size
      if(print_comment) then
        write(unit=print_unit, fmt='(A,A)') "# ", trim(this%named_arguments(i)%help)
      end if
      write(unit=print_unit, fmt='(A,"=",A)') trim(this%named_arguments(i)%name), trim(this%named_arguments(i)%value)
    end do
    if(this%argument_size > 0) then
      write(unit=print_unit, fmt='(A)') "[arguments]"
    end if
    do i = 1,this%argument_size
      if(print_comment) then
        write(unit=print_unit, fmt='(A,A)') "# ", trim(this%arguments(i)%help)
      end if
      write(unit=print_unit, fmt='(A,"=",A)') trim(this%arguments(i)%name), trim(this%arguments(i)%value)
    end do
  end subroutine

  subroutine argp_add_sc_option(this, short_name, long_name, help, callback)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    external :: callback
    integer :: t_sc_size, idx
    type(short_circuit_option), dimension(:), allocatable :: t_sc_opts
    ! long name must not be empty
    call argp_check_long_name(this, long_name)
    ! allow short name to be empty
    if(short_name /= "") then
      call argp_check_short_name(this, short_name)
      idx = ichar(short_name(2:2))
      this%short_name_index(idx) = this%sc_option_size + 1
    end if
    ! 手动管理变长数组
    t_sc_size = size(this%sc_options, 1)
    if(t_sc_size == this%sc_option_size) then
      allocate(t_sc_opts(t_sc_size))
      t_sc_opts(1:t_sc_size) = this%sc_options
      deallocate(this%sc_options)
      allocate(this%sc_options(2 * t_sc_size))
      this%sc_options(1:t_sc_size) = t_sc_opts
      deallocate(t_sc_opts)
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

  subroutine argp_try_add_option(this, short_name, long_name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    integer :: t_opt_size, idx
    type(option), dimension(:), allocatable :: t_opts
    call argp_check_long_name(this, long_name)
    if(short_name /= "") then
      call argp_check_short_name(this, short_name)
      idx = ichar(short_name(2:2))
      this%short_name_index(idx) = this%option_size + 1
    end if
    ! 手动管理变长数组
    t_opt_size = size(this%options, 1)
    if(t_opt_size == this%option_size) then
      allocate(t_opts(t_opt_size))
      t_opts(1:t_opt_size) = this%options
      deallocate(this%options)
      allocate(this%options(2 * t_opt_size))
      this%options(1:t_opt_size) = t_opts
      deallocate(t_opts)
    end if
    this%option_size = this%option_size + 1
    idx = this%option_size
    this%options(idx)%short_name = short_name
    this%options(idx)%long_name = long_name
    this%options(idx)%help = help
  end subroutine argp_try_add_option

  subroutine argp_add_option_logical(this, short_name, long_name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    integer :: idx
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "logical"
    this%options(idx)%value = "false"
  end subroutine argp_add_option_logical

  subroutine argp_add_option_integer(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    integer, intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "integer"
    write(unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_integer

  subroutine argp_add_option_real(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    real, intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "real"
    write(unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_real

  subroutine argp_add_option_double(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    real(kind=8), intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "double"
    write(unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_double

  subroutine argp_add_option_string(this, short_name, long_name, help, default)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: short_name, long_name, help
    character(len=*), intent(in) :: default
    integer :: idx
    character(len=value_len) :: value_buffer
    call argp_try_add_option(this, short_name, long_name, help)
    idx = this%option_size
    this%options(idx)%value_type = "string"
    write(unit=value_buffer, fmt=*) default
    this%options(idx)%value = adjustl(value_buffer)
  end subroutine argp_add_option_string

  subroutine argp_try_add_argument(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: t_arg_size, idx
    type(argument), dimension(:), allocatable :: t_args
    call argp_check_argument_name(this, name)
    ! 手动管理变长数组
    t_arg_size = size(this%arguments, 1)
    if(t_arg_size == this%argument_size) then
      allocate(t_args(t_arg_size))
      t_args(:) = this%arguments
      deallocate(this%arguments)
      allocate(this%arguments(2 * t_arg_size))
      this%arguments(1:t_arg_size) = t_args
      deallocate(t_args)
    end if
    this%argument_size = this%argument_size + 1
    idx = this%argument_size
    this%arguments(idx)%name = name
    this%arguments(idx)%help = help
  end subroutine argp_try_add_argument

  subroutine argp_try_add_named_argument(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: t_arg_size, idx
    type(argument), dimension(:), allocatable :: t_args
    call argp_check_argument_name(this, name)
    ! 手动管理变长数组
    t_arg_size = size(this%named_arguments, 1)
    if(t_arg_size == this%named_argument_size) then
      allocate(t_args(t_arg_size))
      t_args(:) = this%named_arguments
      deallocate(this%named_arguments)
      allocate(this%named_arguments(2 * t_arg_size))
      this%named_arguments(1:t_arg_size) = t_args
      deallocate(t_args)
    end if
    this%named_argument_size = this%named_argument_size + 1
    idx = this%named_argument_size
    this%named_arguments(idx)%name = name
    this%named_arguments(idx)%help = help
  end subroutine argp_try_add_named_argument

  subroutine argp_add_argument_integer(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "integer"
  end subroutine argp_add_argument_integer

  subroutine argp_add_argument_real(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "real"
  end subroutine argp_add_argument_real

  subroutine argp_add_argument_double(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "double"
  end subroutine argp_add_argument_double

  subroutine argp_add_argument_string(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_argument(this, name, help)
    idx = this%argument_size
    this%arguments(idx)%value_type = "string"
  end subroutine argp_add_argument_string

  subroutine argp_add_named_argument_integer(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "integer"
  end subroutine argp_add_named_argument_integer

  subroutine argp_add_named_argument_real(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "real"
  end subroutine argp_add_named_argument_real

  subroutine argp_add_named_argument_double(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "double"
  end subroutine argp_add_named_argument_double

  subroutine argp_add_named_argument_string(this, name, help)
    class(argparser), intent(inout) :: this
    character(len=*), intent(in) :: name, help
    integer :: idx
    call argp_try_add_named_argument(this, name, help)
    idx = this%named_argument_size
    this%named_arguments(idx)%value_type = "string"
  end subroutine argp_add_named_argument_string

  integer function argp_find_option(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    do i = 1,this%option_size
      if(name == this%options(i)%short_name .or. name == this%options(i)%long_name) then
        ans = i
        return
      end if
    end do
    ans = 0
  end function argp_find_option

  logical function argp_get_option_logical(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    if(i == 0) then
      error stop "(get error) option not found: "//trim(name)
    end if
    read(unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_logical

  logical function argp_has_option(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    ans = argp_get_option_logical(this, name)
  end function argp_has_option

  integer function argp_get_option_integer(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    if(i == 0) then
      error stop "(get error) option not found: "//trim(name)
    end if
    read(unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_integer

  real function argp_get_option_real(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    if(i == 0) then
      error stop "(get error) option not found: "//trim(name)
    end if
    read(unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_real

  real(kind=8) function argp_get_option_double(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_option(this, name)
    if(i == 0) then
      error stop "(get error) option not found: "//trim(name)
    end if
    read(unit=this%options(i)%value, fmt=*) ans
  end function argp_get_option_double

  function argp_get_option_string(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    character(len=value_len) :: ans
    integer :: i
    i = argp_find_option(this, name)
    if(i == 0) then
      error stop "(get error) option not found: "//trim(name)
    end if
    ans = this%options(i)%value
  end function argp_get_option_string

  integer function argp_find_argument(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    do i = 1,this%named_argument_size
      if(name == this%named_arguments(i)%name) then
        ans = i
        return
      end if
    end do
    do i = 1,this%argument_size
      if(name == this%arguments(i)%name) then
        ans = -i
        return
      end if
    end do
    ans = 0
  end function argp_find_argument

  integer function argp_get_argument_integer(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_argument(this, name)
    if(i == 0) then
      error stop "(get error) argument not found: "//trim(name)
    else if(i > 0) then
      read(unit=this%named_arguments(i)%value, fmt=*) ans
    else
      read(unit=this%arguments(-i)%value, fmt=*) ans
    end if
  end function argp_get_argument_integer

  real function argp_get_argument_real(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_argument(this, name)
    if(i == 0) then
      error stop "(get error) argument not found: "//trim(name)
    else if(i > 0) then
      read(unit=this%named_arguments(i)%value, fmt=*) ans
    else
      read(unit=this%arguments(-i)%value, fmt=*) ans
    end if
  end function argp_get_argument_real

  real(kind=8) function argp_get_argument_double(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: i
    i = argp_find_argument(this, name)
    if(i == 0) then
      error stop "(get error) argument not found: "//trim(name)
    else if(i > 0) then
      read(unit=this%named_arguments(i)%value, fmt=*) ans
    else
      read(unit=this%arguments(-i)%value, fmt=*) ans
    end if
  end function argp_get_argument_double

  function argp_get_argument_string(this, name) result(ans)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    character(len=value_len) :: ans
    integer :: i
    i = argp_find_argument(this, name)
    if(i == 0) then
      error stop "(get error) argument not found: "//trim(name)
    else if(i > 0) then
      ans = this%named_arguments(i)%value
    else
      ans = this%arguments(-i)%value
    end if
  end function argp_get_argument_string

  subroutine argp_check_short_name(this, name)
    class(argparser), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: name_size, char_pos
    name_size = len(name)
    if(name_size /= 2 .or. name(1:1) /= '-') then
      error stop "(build error) short option name must be `-` followed by one character"
    end if
    char_pos = ichar(name(2:2))
    if(this%short_name_index(char_pos) /= -1) then
      print "(A,A,A)", "(build error) short option name ", name, " already exists"
      error stop
    end if
  end subroutine argp_check_short_name

  subroutine argp_check_long_name(this, name)
    class(argparser), intent(in) :: this
    character(len=*) :: name
    integer :: i
    if(name == "") then
      error stop "(build error) long option name cannot be empty"
    end if
    if(name(1:2) /= "--") then
      error stop "(build error) long option name must be `--` followed by one or more characters"
    end if
    do i = 1, this%sc_option_size
      if(name == trim(this%sc_options(i)%long_name)) then
        print "(A,A,A)", "(build error) long option name ", name, " already exists"
        error stop
      end if
    end do
    do i = 1, this%option_size
      if(name == trim(this%options(i)%long_name)) then
        print "(A,A,A)", "(build error) long option name ", name, " already exists"
        error stop
      end if
    end do
  end subroutine argp_check_long_name

  subroutine argp_check_argument_name(this, name)
    class(argparser), intent(in) :: this
    character(len=*) :: name
    integer :: i
    if(name == "") then
      error stop "(build error) argument name cannot be empty"
    end if
    do i = 1,this%argument_size
      if(name == trim(this%arguments(i)%name)) then
        print "(A,A,A)", "(build error) argument name ", name, " already exists"
        error stop
      end if
    end do
    do i = 1,this%named_argument_size
      if(name == trim(this%named_arguments(i)%name)) then
        print "(A,A,A)", "(build error) argument name ", name, " already exists"
        error stop
      end if
    end do
  end subroutine argp_check_argument_name

  subroutine split(line, sep, result)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: sep
    character(len=*), dimension(:), allocatable, intent(out) :: result
    integer :: i, di, line_size, res_count
    line_size = len_trim(line)
    res_count = 1
    i = index(line(1:line_size), sep)
    di = i
    do while(di /= 0)
      res_count = res_count + 1
      di = index(line(i+1:line_size), sep)
      i = i + di
    end do
    allocate(result(res_count))
    res_count = 1
    i = index(line(1:line_size), sep)
    di = i
    do while(di /= 0)
      result(res_count) = line(i-di+1:i-1)
      res_count = res_count + 1
      di = index(line(i+1:line_size), sep)
      i = i + di
    end do
    result(res_count) = line(i+1:line_size)
  end subroutine
end module argparse
