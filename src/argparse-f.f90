module argparse_f
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, argparse-f!"
  end subroutine say_hello
end module argparse_f
