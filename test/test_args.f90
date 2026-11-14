program test_args
    !! The command line as XFCE and xfce4-terminal users pass it.
    use fortterm_args, only: options_t, parse_options
    implicit none

    type(options_t) :: o
    integer :: failures = 0

    call parse_options([character(len=32) :: "--working-directory=/tmp", "-x", "htop", "-d", &
        "5"], o)
    call check(o%cwd == "/tmp", "--working-directory=DIR")
    call check(size(o%command) == 3, "-x takes the rest")
    if (size(o%command) == 3) call check(o%command(1) == "htop" .and. o%command(3) == "5", &
        "-x arguments kept")

    call parse_options([character(len=32) :: "-e", "ls -l | less", "--hold", "-T", "List"], o)
    call check(size(o%command) == 3, "-e runs through the shell")
    if (size(o%command) == 3) call check(o%command(1) == "/bin/sh" .and. &
        o%command(3) == "ls -l | less", "-e command line")
    call check(o%hold .and. o%title == "List", "--hold and -T")

    call parse_options([character(len=32) :: "--disable-server", "--geometry", "80x24", &
        "--font-size=12"], o)
    call check(o%ignored == 2 .and. size(o%command) == 0, "xfce4-terminal options ignored")
    call check(o%font_size == 12.0, "--font-size")

    call parse_options([character(len=32) :: "--title"], o)
    call check(len(o%error) > 0, "missing value reported")

    if (failures > 0) error stop "argument tests failed"
    print *, "fortterm argument tests passed"

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        if (.not. ok) then
            print *, "FAIL: ", what
            failures = failures + 1
        end if
    end subroutine check

end program test_args
