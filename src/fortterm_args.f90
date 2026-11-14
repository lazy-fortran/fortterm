module fortterm_args
    !! Command line of fortterm, close to xfce4-terminal's so that it can
    !! replace it: -x/--execute runs the remaining arguments, -e/--command a
    !! command line through the shell, --working-directory, -T/--title, -H/--hold.
    implicit none
    private

    public :: options_t, parse_options, usage

    type :: options_t
        character(len=1024), allocatable :: command(:)
        character(len=:), allocatable :: cwd, title, colors, error
        logical :: hold = .false.
        logical :: help = .false.
        logical :: version = .false.
        ! Wait hidden for the next start (fortterm starts these itself).
        logical :: spare = .false.
        real :: font_size = 0.0
        ! Options of xfce4-terminal that mean nothing here.
        integer :: ignored = 0
    end type options_t

contains

    subroutine parse_options(args, opts)
        !! Interpret `args` (the program's arguments without its name).
        character(*), intent(in) :: args(:)
        type(options_t), intent(out) :: opts
        integer :: k, eq, ios
        character(len=:), allocatable :: a, name, value
        logical :: has_value

        allocate(opts%command(0))
        opts%cwd = ""
        opts%title = ""
        opts%colors = ""
        opts%error = ""
        k = 1
        do while (k <= size(args))
            a = trim(args(k))
            if (a == "-x" .or. a == "--execute") then
                if (k < size(args)) opts%command = args(k + 1:)
                return
            end if
            has_value = .false.
            value = ""
            name = a
            if (len(a) > 2) then
                if (a(1:2) == "--") then
                    eq = index(a, "=")
                    if (eq > 0) then
                        name = a(1:eq - 1)
                        value = a(eq + 1:)
                        has_value = .true.
                    end if
                end if
            end if
            select case (name)
            case ("-h", "--help")
                opts%help = .true.
            case ("-V", "--version")
                opts%version = .true.
            case ("-H", "--hold")
                opts%hold = .true.
            case ("--spare")
                opts%spare = .true.
            case ("-e", "--command")
                call take(value)
                opts%command = [character(len=1024) :: "/bin/sh", "-c", value]
            case ("--working-directory", "--default-working-directory")
                call take(value)
                opts%cwd = value
            case ("-T", "--title", "--initial-title")
                call take(value)
                opts%title = value
            case ("--font-size")
                call take(value)
                read (value, *, iostat=ios) opts%font_size
                if (ios /= 0 .or. opts%font_size <= 0.0) then
                    opts%error = "--font-size needs a positive number"
                    opts%font_size = 0.0
                end if
            case ("--colors")
                call take(value)
                opts%colors = value
            case ("--tab", "--window", "--disable-server", "--maximize", "--fullscreen", &
                    "--show-menubar", "--hide-menubar", "--show-borders", "--hide-borders", &
                    "--show-toolbar", "--hide-toolbar")
                opts%ignored = opts%ignored + 1
            case ("--geometry", "--role", "--startup-id", "--display", "--icon", "-I", &
                    "--zoom", "--active-tab")
                call take(value)
                opts%ignored = opts%ignored + 1
            case default
                if (len(a) > 0) then
                    if (a(1:1) == "-") then
                        opts%ignored = opts%ignored + 1
                    else
                        ! A bare word: a program to run, as some launchers pass it.
                        opts%command = args(k:)
                        return
                    end if
                end if
            end select
            k = k + 1
        end do
    contains
        subroutine take(v)
            character(len=:), allocatable, intent(inout) :: v
            if (has_value) return
            if (k < size(args)) then
                k = k + 1
                v = trim(args(k))
            else
                opts%error = name // " needs a value"
                v = ""
            end if
        end subroutine take
    end subroutine parse_options

    function usage() result(text)
        character(len=:), allocatable :: text
        character(len=*), parameter :: nl = new_line("a")
        text = "usage: fortterm [options] [-x program [arguments]]" // nl // nl // &
            "  -x, --execute PROGRAM ARGS  run PROGRAM with the remaining arguments" // nl // &
            "  -e, --command ""LINE""        run a command line through /bin/sh" // nl // &
            "  --working-directory DIR     start in DIR" // nl // &
            "  -T, --title TITLE           window title (else the program's)" // nl // &
            "  -H, --hold                  keep the window when the program ends" // nl // &
            "  --font-size N               text size in points (default 14)" // nl // &
            "  --colors dark|light         colour scheme (default dark)" // nl // nl // &
            "Keys: Ctrl+Shift+C copy, Ctrl+Shift+V paste, Ctrl+Shift+T new tab," // nl // &
            "Ctrl+Shift+W close tab, Ctrl+Shift+Left/Right switch tabs," // nl // &
            "Ctrl+Shift+= / Ctrl+Shift+- text size, Shift+Page Up/Down scrollback." // nl
    end function usage

end module fortterm_args
