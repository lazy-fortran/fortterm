program fortterm
    !! A terminal emulator on FortUI: tabs of terminals that show kitty and
    !! sixel images, in a window (SDL3 GPU) or inside another terminal.
    use iso_fortran_env, only: error_unit, int32
    use fortui
    use fortui_backends, only: ui_init
    use fortui_term, only: term_t, term_close, term_pump, term_available
    use fortui_terminal, only: terminal, terminal_view_t
    use fortterm_args, only: options_t, parse_options, usage
    implicit none

    integer, parameter :: MAX_TABS = 16
    character(len=*), parameter :: VERSION = "0.1.0"
    ! Terminals stay in place while they run (libvterm keeps their address).
    type(term_t), target, save :: terms(MAX_TABS)
    type(terminal_view_t), save :: views(MAX_TABS)
    logical :: used(MAX_TABS) = .false.
    character(len=1024), allocatable :: args(:)
    type(options_t) :: opts
    character(len=:), allocatable :: shown_title, title, tab_labels(:)
    integer(int32) :: fg, bg, palette(16)
    integer :: k, n, active, stat, ntabs, selected, focused_tab
    real :: font_size

    n = command_argument_count()
    allocate(args(n))
    do k = 1, n
        call get_command_argument(k, args(k))
    end do
    call parse_options(args, opts)
    if (opts%help) then
        write (*, '(a)', advance="no") usage()
        stop
    end if
    if (opts%version) then
        print '(a)', "fortterm " // VERSION
        stop
    end if
    if (len(opts%error) > 0) then
        write (error_unit, '(a)') "fortterm: " // opts%error
        error stop 2
    end if
    if (.not. term_available()) error stop "fortterm: FortUI was built without its terminal"
    font_size = 14.0
    if (opts%font_size > 0.0) font_size = opts%font_size
    call colours(opts%colors, fg, bg, palette)

    call ui_init("auto", 800, 520, "fortterm", stat=stat)
    if (stat /= 0) error stop "fortterm: no display and no terminal available"
    call ui_set_window_margin(0.0)
    call open_tab(.true.)
    active = 1
    focused_tab = 0
    shown_title = ""

    do while (.not. ui_should_close())
        call ui_frame_begin()
        if (ui_shortcut("ctrl+shift+t", label="New tab")) call open_tab(.false.)
        if (ui_shortcut("ctrl+shift+w", label="Close tab")) call close_tab(active)
        if (ui_shortcut("ctrl+shift+arrowright", repeat=.true.)) active = next_tab(active, 1)
        if (ui_shortcut("ctrl+shift+arrowleft", repeat=.true.)) active = next_tab(active, -1)
        if (ui_shortcut("ctrl+shift+=", repeat=.true.)) font_size = min(48.0, font_size + 1.0)
        if (ui_shortcut("ctrl+shift+-", repeat=.true.)) font_size = max(6.0, font_size - 1.0)
        if (ui_shortcut("ctrl+shift+0")) font_size = 14.0
        ntabs = count(used)
        if (ntabs > 1) then
            call labels()
            selected = position(active)
            if (tabs("tabs", tab_labels, selected)) active = nth_tab(selected)
        end if
        call terminal("term" // itoa(active), terms(active), views(active), font_size, &
            UI_FILL, UI_FILL, opts%command, opts%cwd)
        ! The shown terminal has the keyboard.
        if (focused_tab /= active) then
            call ui_focus("term" // itoa(active))
            focused_tab = active
        end if
        ! Terminals in other tabs keep reading what their programs write.
        do k = 1, MAX_TABS
            if (used(k) .and. k /= active .and. views(k)%started) then
                if (term_pump(terms(k))) continue
            end if
        end do
        call ui_present()

        title = opts%title
        if (len(title) == 0 .and. allocated(terms(active)%title)) title = terms(active)%title
        if (len(title) == 0) title = "fortterm"
        if (title /= shown_title) then
            call ui_set_title(title)
            shown_title = title
        end if
        do k = 1, MAX_TABS
            if (used(k) .and. views(k)%started .and. .not. terms(k)%alive .and. &
                terms(k)%exit_status >= 0 .and. .not. opts%hold) call close_tab(k)
        end do
        if (count(used) == 0) exit
        if (.not. used(active)) active = next_tab(active, 1)
    end do
    do k = 1, MAX_TABS
        if (used(k)) call term_close(terms(k))
    end do
    call ui_shutdown()

contains

    subroutine open_tab(first)
        !! A new tab running the command line's program (the first tab) or
        !! the shell (later ones).
        logical, intent(in) :: first
        integer :: j
        do j = 1, MAX_TABS
            if (used(j)) cycle
            used(j) = .true.
            views(j) = terminal_view_t()
            terms(j)%default_fg = fg
            terms(j)%default_bg = bg
            terms(j)%palette = palette
            terms(j)%own_palette = .true.
            if (.not. first) then
                if (allocated(opts%command)) deallocate(opts%command)
                allocate(opts%command(0))
            end if
            active = j
            return
        end do
    end subroutine open_tab

    subroutine close_tab(j)
        integer, intent(in) :: j
        if (.not. used(j)) return
        call term_close(terms(j))
        used(j) = .false.
    end subroutine close_tab

    integer function next_tab(from, step) result(j)
        integer, intent(in) :: from, step
        integer :: i
        j = from
        do i = 1, MAX_TABS
            j = modulo(j - 1 + step, MAX_TABS) + 1
            if (used(j)) return
        end do
        j = from
    end function next_tab

    integer function position(j) result(p)
        integer, intent(in) :: j
        p = count(used(1:j))
    end function position

    integer function nth_tab(p) result(j)
        integer, intent(in) :: p
        integer :: seen
        seen = 0
        do j = 1, MAX_TABS
            if (used(j)) seen = seen + 1
            if (seen == p) return
        end do
        j = active
    end function nth_tab

    subroutine labels()
        integer :: j, m
        character(len=:), allocatable :: s
        if (allocated(tab_labels)) deallocate(tab_labels)
        allocate(character(len=40) :: tab_labels(count(used)))
        m = 0
        do j = 1, MAX_TABS
            if (.not. used(j)) cycle
            m = m + 1
            s = ""
            if (allocated(terms(j)%title)) s = terms(j)%title
            if (len(s) == 0) s = "Terminal " // itoa(m)
            tab_labels(m) = s
        end do
    end subroutine labels

    subroutine colours(name, fg, bg, palette)
        !! Dark (default) or light scheme; the 16 ANSI colours are Tango's, the
        !! palette most Linux terminals start with.
        character(*), intent(in) :: name
        integer(int32), intent(out) :: fg, bg, palette(16)
        integer :: j
        integer, parameter :: tango(3, 16) = reshape([ &
            46, 52, 54, 204, 0, 0, 78, 154, 6, 196, 160, 0, 52, 101, 164, 117, 80, 123, &
            6, 152, 154, 211, 215, 207, 85, 87, 83, 239, 41, 41, 138, 226, 52, 252, 233, 79, &
            114, 159, 207, 173, 127, 168, 52, 226, 226, 238, 238, 236], [3, 16])
        do j = 1, 16
            palette(j) = ui_rgba(tango(1, j), tango(2, j), tango(3, j))
        end do
        if (name == "light") then
            fg = ui_rgba(36, 36, 36)
            bg = ui_rgba(255, 255, 255)
        else
            fg = ui_rgba(212, 212, 212)
            bg = ui_rgba(30, 30, 30)
        end if
    end subroutine colours

    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, '(i0)') i
        s = trim(buf)
    end function itoa

end program fortterm
