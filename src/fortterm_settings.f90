module fortterm_settings
    !! The user's xfce4-terminal settings, so that fortterm looks the same: the
    !! xfconf channel xfce4-terminal (xfce4-terminal 1.1 and later), else the
    !! older terminalrc, and the screen resolution XFCE uses for point sizes.
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: settings_t, read_settings, parse_colour, parse_font

    type :: settings_t
        logical :: found = .false.
        character(len=:), allocatable :: font_family
        real :: font_points = 0.0
        real :: dpi = 96.0
        logical :: have_fg = .false., have_bg = .false., have_cursor = .false.
        logical :: have_palette = .false.
        integer(int32) :: fg = 0, bg = 0, cursor = 0, palette(16) = 0
        ! 1 block, 2 bar (I-beam), 3 underline, as fortui_term's CURSOR_*.
        integer :: cursor_shape = 1
        integer :: cols = 0, rows = 0
        ! "Show bold text in bright colors" (xfce4-terminal's default: on).
        logical :: bold_bright = .true.
    end type settings_t

contains

    subroutine read_settings(s)
        type(settings_t), intent(out) :: s
        character(len=:), allocatable :: home, base
        character(len=512) :: env
        integer :: n, st

        s%font_family = ""
        call get_environment_variable("XDG_CONFIG_HOME", env, n, st)
        if (st == 0 .and. n > 0) then
            base = trim(env)
        else
            call get_environment_variable("HOME", env, n, st)
            if (st /= 0) return
            home = trim(env)
            base = home // "/.config"
        end if
        call read_xfconf(base // "/xfce4/xfconf/xfce-perchannel-xml/xfce4-terminal.xml", s)
        if (.not. s%found) call read_terminalrc(base // "/xfce4/terminal/terminalrc", s)
        call read_dpi(base // "/xfce4/xfconf/xfce-perchannel-xml/xsettings.xml", s)
    end subroutine read_settings

    subroutine read_xfconf(path, s)
        character(*), intent(in) :: path
        type(settings_t), intent(inout) :: s
        character(len=4096) :: line
        character(len=:), allocatable :: name, value
        integer :: u, ios
        open (newunit=u, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            name = attribute(line, "name")
            value = attribute(line, "value")
            if (len(name) == 0) cycle
            call apply(s, name, value)
        end do
        close (u)
    end subroutine read_xfconf

    subroutine read_terminalrc(path, s)
        character(*), intent(in) :: path
        type(settings_t), intent(inout) :: s
        character(len=4096) :: line
        integer :: u, ios, eq
        character(len=:), allocatable :: key
        open (newunit=u, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            eq = index(line, "=")
            if (eq == 0) cycle
            select case (line(1:eq - 1))
            case ("FontName"); key = "font-name"
            case ("ColorForeground"); key = "color-foreground"
            case ("ColorBackground"); key = "color-background"
            case ("ColorCursor"); key = "color-cursor"
            case ("ColorPalette"); key = "color-palette"
            case ("MiscCursorShape"); key = "misc-cursor-shape"
            case ("MiscDefaultGeometry"); key = "misc-default-geometry"
            case ("ColorBoldIsBright"); key = "color-bold-is-bright"
            case default; cycle
            end select
            call apply(s, key, trim(line(eq + 1:)))
        end do
        close (u)
    end subroutine read_terminalrc

    subroutine apply(s, name, value)
        type(settings_t), intent(inout) :: s
        character(*), intent(in) :: name, value
        integer :: k, a, b, x
        logical :: ok
        select case (name)
        case ("font-name")
            call parse_font(value, s%font_family, s%font_points)
            s%found = .true.
        case ("color-foreground")
            call parse_colour(value, s%fg, s%have_fg)
            s%found = .true.
        case ("color-background")
            call parse_colour(value, s%bg, s%have_bg)
            s%found = .true.
        case ("color-cursor")
            call parse_colour(value, s%cursor, s%have_cursor)
        case ("color-palette")
            a = 1
            s%have_palette = .true.
            do k = 1, 16
                b = index(value(a:), ";")
                if (b == 0) then
                    b = len(value) + 1
                else
                    b = a + b - 1
                end if
                call parse_colour(value(a:b - 1), s%palette(k), ok)
                if (.not. ok) s%have_palette = .false.
                a = b + 1
                if (a > len(value) .and. k < 16) s%have_palette = .false.
                if (a > len(value)) exit
            end do
        case ("color-bold-is-bright", "ColorBoldIsBright")
            s%bold_bright = index(value, "rue") > 0 .or. index(value, "RUE") > 0
        case ("misc-cursor-shape")
            if (index(value, "IBEAM") > 0) s%cursor_shape = 2
            if (index(value, "UNDERLINE") > 0) s%cursor_shape = 3
        case ("misc-default-geometry")
            x = index(value, "x")
            if (x > 1) then
                read (value(1:x - 1), *, iostat=k) a
                if (k == 0) s%cols = a
                b = verify(value(x + 1:), "0123456789")
                if (b == 0) b = len(value(x + 1:)) + 1
                read (value(x + 1:x + b - 1), *, iostat=k) a
                if (k == 0) s%rows = a
            end if
        end select
    end subroutine apply

    subroutine read_dpi(path, s)
        !! Xft/DPI of xsettings (-1 or missing: 96, as GTK assumes).
        character(*), intent(in) :: path
        type(settings_t), intent(inout) :: s
        character(len=1024) :: line
        integer :: u, ios, v
        logical :: in_xft
        character(len=:), allocatable :: text
        open (newunit=u, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        in_xft = .false.
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (attribute(line, "name") == "Xft") in_xft = .true.
            if (in_xft .and. attribute(line, "name") == "DPI") then
                text = attribute(line, "value")
                read (text, *, iostat=ios) v
                if (ios == 0 .and. v > 0) s%dpi = real(v)
                exit
            end if
        end do
        close (u)
    end subroutine read_dpi

    function attribute(line, key) result(v)
        !! The value of key="..." in an XML tag on this line.
        character(*), intent(in) :: line, key
        character(len=:), allocatable :: v
        integer :: a, b
        v = ""
        a = index(line, " " // key // '="')
        if (a == 0) return
        a = a + len(key) + 3
        b = index(line(a:), '"')
        if (b == 0) return
        v = line(a:a + b - 2)
    end function attribute

    subroutine parse_font(name, family, points)
        !! "More Perfect DOS VGA 15" -> family and size in points (Pango style).
        character(*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: family
        real, intent(out) :: points
        integer :: sp, ios
        family = trim(name)
        points = 0.0
        sp = index(trim(name), " ", back=.true.)
        if (sp == 0) return
        read (name(sp + 1:), *, iostat=ios) points
        if (ios /= 0 .or. points <= 0.0) then
            points = 0.0
            return
        end if
        family = trim(name(1:sp - 1))
    end subroutine parse_font

    subroutine parse_colour(text, rgba, ok)
        !! "#rgb", "#rrggbb" or GDK's "#rrrrggggbbbb" into ui_rgba (opaque).
        character(*), intent(in) :: text
        integer(int32), intent(out) :: rgba
        logical, intent(out) :: ok
        character(len=:), allocatable :: h
        integer :: n, r, g, b, ios, w
        rgba = 0
        ok = .false.
        h = trim(adjustl(text))
        if (len(h) < 4) return
        if (h(1:1) /= "#") return
        h = h(2:)
        n = len(h)
        if (mod(n, 3) /= 0 .or. n > 12) return
        w = n / 3
        read (h(1:min(2, w)), '(z2)', iostat=ios) r
        if (ios /= 0) return
        read (h(w + 1:w + min(2, w)), '(z2)', iostat=ios) g
        if (ios /= 0) return
        read (h(2 * w + 1:2 * w + min(2, w)), '(z2)', iostat=ios) b
        if (ios /= 0) return
        if (w == 1) then
            r = 17 * r
            g = 17 * g
            b = 17 * b
        end if
        rgba = int(r, int32) + ishft(int(g, int32), 8) + ishft(int(b, int32), 16) + &
            ishft(-1_int32, 24)
        ok = .true.
    end subroutine parse_colour

end module fortterm_settings
