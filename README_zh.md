# argparse-f

现代Fortran的命令行参数解析器，使用了面向对象特性。

## 示例
```fortran
program test
    use iso_fortran_env, only: stdout => output_unit
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
    ! print '(/,A)', "All of the options and arguments are shown below"
    ! call args%print_as_ini(stdout, .true.)
contains
    subroutine show_version_info
        print "(A)", trim(program_name)//" version 0.1.0"
    end subroutine show_version_info
end program test
```
将其编译成`qclac`，使用方式如下
```bash
> qclac -?
usage: qcalc [options] [=input] [=output] 

A quantum physics calculation program.

Options:
  -?, --help               show this help message
  -v, --version            show version info
  -o, --openmp             use openmp or not
  -m, --mpi                use mpi or not
  -t, --thread             (integer) thread number,
                           it is valid only if openmp is set
  -p, --process            (integer) process number,
                           it is valid only if mpi is set
  --chemical               (string) chemical formula

Named arguments:
  input                    (string) initialize file
  output                   (string) output file
> qclac -om -t 16 input=input.txt output=out.bin 
openmp is used, and we use 16 threads
mpi is used, and we use  1 processes
the calculated chemical is H2O
the input file is input.txt
the output file is out.bin
```

## 使用

最简单的方法是复制`src/argparse-f.f90`文件到你的项目中。推荐使用[fpm](https://fpm.fortran-lang.org/en/index.html)
```toml
[dependencies]
argparse-f = { git="https://github.com/0382/argparse-f.git" }
```

## 解析规则

在我的库里面，命令行参数分为两大类（可选的选项，和必选的参数），每类又分为两种，总共四种命令行参数。

### `option`（选项）

选项，从语义上来说是可选的。它分成两种：一般选项和短路选项。

#### 一般选项

用如下方式添加一般选项：
```fortran
call args%add_option_integer("-t", "--thread", "thread number", 1)
```

其中第一个参数是短选项名，是可以为空字符串的，如果非空，则必须是一个'-'后接**单个**字符；第二个是长选项名，不能为空字符串，必须以"--"开头。第三个是帮助信息，最后一个是该选项的默认值。

当前版本一般选项仅支持五种数据类型：`logical, integer, real, real(kind=8), character(len=*)` （添加`real(8)`的方法是`add_option_double`，添加`character(len=*)`的方法是`add_option_string`）。

除了bool型的option，其余的option添加时都要给定默认的值。
对于bool型，不需要默认值，检查到命令行参数有这个选项，就是true否则为false。例如
```bash
ls -l
```
此时-l选项为true。而其他类型选项需要在其后面加上参数，比如
```bash
greet --name Alice --age 24
```
于是`--name`选项的值为`"Alice"`，`--age`选项的值为`24`。

#### 短路选项

短路选项（short circuit option）按照如下方式添加
```fortran
call args%add_sc_option("-v", "--version", "show version info", show_version_info)
contains
subroutine show_version_info
    print "(A)", trim(program_name)//" version 0.1.0"
end subroutine show_version_info
```

短路选项仅支持bool类型，添加该选项时需要给定一个回调函数，必须是无参数的`subroutine`。短路选项是最先搜索解析一种命令行参数。比如
```bash
git --help
gcc -v
```
只要命令行参数包含了这类参数，则调用回调函数，并立即（正常）退出程序。

### `argument`（参数）

参数，和选项相反是必须提供的。如果某个参数没有提供，则程序会报错并退出。参数分为位置参数和命名参数两种

当前版本参数仅支持四种数据类型：`integer, real, real(kind=8), character(len=*)`。不支持`logical`类型，实际上`logical`类型用选项是更合适的。

#### 位置参数

按照位置获取的参数，例如
```bash
ffplay video.mp4
```
`video.mp4`就是一个位置参数。如果使用`ffplay`程序没有指定这个位置参数，那么程序就发生错误并退出。

按照如下方式添加位置参数
```fortran
call args%add_argument_string("input", "initialize file")
```
该函数的第一个哑元表示参数的名字，第二个表示帮助信息。

#### 命名参数

这是为了解决我个人工作中遇到的情况而定义的。它的使用方式例如
```bash
greet name=Alice age=24
```
显然这样使用参数非常繁琐，不应该作为轻量级的命令行程序使用。但是可以放在一个较重的工程中，并且运行的时候是用脚本调用程序而不是直接在命令行使用。这个时候，使用命名参数可以让你的脚本更具可读性。

使用如下方式添加命名参数
Add named argument like this:
```fortran
call args%add_named_argument_string("input", "initialize file")
```

解析命令行参数时，先解析命名参数，剩下的自动按照顺序赋值给位置参数。命名参数不必按照设置的顺序指定。

## 获取结果

### 参数

使用类似`args%get_option_logical(name)`的方法获取结果选项的结果，这里的`name`既可以长名字也可以短名字。对于`logical`类型的选项，特别提供了`args%has_option(name)`函数。

### argument

使用类似`args%get_argument_logical(name)`的方法获取结果参数的结果。命名参数和位置参数的名字不允许冲突，所以获取的时候可以不用区分，就没有`get_named_argument_xxx`版本的函数了。

## 杂项

### 冲突

短路选项和一般选项的名字不允许冲突。命名参数和位置参数的名字也不允许冲突。

这是为了实现linux一些基本命令行工具类似的效果。比如
```
ls -lah
```
同时指定了`-l`, `-a`和`-h`选项。只有`logical`类型的选项才能够这样指定。

> 这也是为什么选项的短名字仅允许一个字符。

### 自定义帮助选项

使用`args%add_help_option()`可以添加默认的帮助选项，它使用的名字是`-?`和`--help`。如果你不喜欢这个名字，可以添加自定义的帮助选项
```fortran
call args%add_sc_option("-h", "--help", "show this help message", print_help)
contains
subroutine print_help
    call args%print_help()
end subroutine
```

> 默认使用`-?`而不是`-h`是为了给其他的选项留出选择空间。

### 多行的帮助信息

有的时候我们的帮助信息很长，如果写在一行但是控制台的宽度不够造成换行会很难看。你可以在帮助信息里面加上`\n`作为换行标识。当然Fortran是没有转义字符的，不过这不重要。这个包会根据`\n`自动添加换行和空格使得帮助信息更好看。

### 打印获取的结果

你可以将`argparser`获取的结果打印为INI文件格式
```fortran
call args%print_as_ini(stdout, .true.)
```
这个函数的第二个参数表示是否打印注释，如果为`.true.`那么会把帮助信息打印为注释。

### `print_uasge`和`set_program_name`
如果你运行程序不带任何命令行参数，那么`argparser`会调用`print_usage`并退出。`print_usage`实际上就是`print_help`的第一行信息，算是一个简短的帮助信息。`set_program_name`仅仅影响`print_usage`时显示的程序名字，如果不调用这个函数，那么会使用`argv[0]`。

## 参考

这个包和我自己写的c++包[argparse](https://github.com/0382/util/tree/main/cpp/argparse)工作方式是类似的。这两个包都受到了c++包[cmdline](https://github.com/tanakh/cmdline)以及python标准库的[argparse](https://docs.python.org/3/library/argparse.html)模块的启发。