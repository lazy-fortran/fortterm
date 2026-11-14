program fortterm
    !! A terminal emulator on FortUI: tabs of terminals that show kitty and
    !! sixel images, in a window (SDL3 GPU) or inside another terminal.
    use iso_fortran_env, only: error_unit, int32
    use fortui
    use fortui_backends, only: ui_init
    use fortui_term, only: term_t, term_close, term_pump, term_available
    use fortui_terminal, only: terminal, terminal_view_t
    use fortterm_args, only: options_t, parse_options, usage
    use fortterm_settings, only: settings_t, read_settings
    use fortterm_spare, only: spare_path, spare_connect, spare_listen, spare_accept, &
        spare_send, spare_receive, spare_close, spare_unlink, spare_spawn, spare_outdated, &
        spare_request, spare_apply, spare_same_program
    implicit none

    integer, parameter :: MAX_TABS = 16
    character(len=*), parameter :: VERSION = "0.1.0"
    ! Terminals stay in place while they run (their reader threads point into them).
    type(term_t), target, save :: terms(MAX_TABS)
    type(terminal_view_t), save :: views(MAX_TABS)
    logical :: used(MAX_TABS) = .false.
    character(len=1024), allocatable :: args(:)
    type(options_t) :: opts
    character(len=:), allocatable :: shown_title, title, tab_labels(:)
    integer(int32) :: fg, bg, palette(16), cursor_color
    integer :: k, n, active, stat, ntabs, selected, focused_tab, cursor_shape, geo(2)
    real :: font_size, default_size
    logical :: sized
    type(settings_t) :: user
    ! FORTTERM_TIMING=1: seconds from start to the window, the first frame and
    ! the program's first output, on standard error.
    logical :: timing, first_frame, first_output
    double precision :: started
    ! Hand-over to a waiting spare instance.
    integer :: conn, lfd
    logical :: got, spawn_spare
    character(len=:), allocatable :: request, answer, req_cwd
    real :: cw, ch

    started = ui_time()
    timing = env_set("FORTTERM_TIMING")
    first_frame = .true.
    first_output = .true.
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
    ! A spare instance waits hidden with its window ready: hand it this start.
    spawn_spare = .false.
    if (.not. opts%spare) then
        conn = spare_connect(spare_path())
        if (conn >= 0) then
            call spare_send(conn, spare_request(args))
            call spare_receive(conn, answer, got)
            call spare_close(conn)
            if (got .and. answer == "ok") then
                if (timing) write (error_unit, '(a,f7.3)') "fortterm: handed to the spare ", &
                    ui_time() - started
                stop
            end if
        end if
        spawn_spare = len(spare_path()) > 0
    else
        ! Another spare already waits: nothing to do.
        conn = spare_connect(spare_path())
        if (conn >= 0) then
            call spare_close(conn)
            stop
        end if
    end if
    ! Look like the user's xfce4-terminal: font, colours, cursor, size.
    call read_settings(user)
    default_size = 14.0
    if (user%font_points > 0.0) default_size = user%font_points * user%dpi / 72.0
    if (len(user%font_family) > 0) call ui_set_cell_font(user%font_family)
    call colours(opts%colors, fg, bg, palette)
    cursor_color = 0
    cursor_shape = user%cursor_shape
    if (len(opts%colors) == 0) then
        if (user%have_fg) fg = user%fg
        if (user%have_bg) bg = user%bg
        if (user%have_palette) palette = user%palette
        if (user%have_cursor) cursor_color = user%cursor
    end if
    if (opts%font_size > 0.0) default_size = opts%font_size
    font_size = default_size
    geo = [80, 24]
    if (user%cols > 0 .and. user%rows > 0) geo = [user%cols, user%rows]
    sized = .false.

    call ui_init("auto", nint(0.5 * font_size) * geo(1), nint(font_size) * geo(2), "fortterm", &
        stat=stat, hidden=opts%spare .or. opts%maximize)
    if (stat /= 0) error stop "fortterm: no display and no terminal available"
    if (opts%spare) call wait_as_spare()
    if (opts%maximize .and. .not. opts%spare) then
        ! Maximized while hidden, then shown: no small window first.
        call ui_maximize_window()
        call ui_set_window_visible(.true.)
        sized = .true.
    end if
    if (timing) write (error_unit, '(a,f7.3)') "fortterm: window ", ui_time() - started
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
        if (ui_shortcut("ctrl+shift+0")) font_size = default_size
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
        if (timing .and. first_frame) &
            write (error_unit, '(a,f7.3)') "fortterm: first frame ", ui_time() - started
        ! Once shown, the next start is prepared.
        if (first_frame .and. spawn_spare) call spare_spawn()
        first_frame = .false.
        if (timing .and. first_output .and. terms(active)%generation > 1) then
            write (error_unit, '(a,f7.3)') "fortterm: program output shown ", ui_time() - started
            first_output = .false.
        end if
        ! The window in whole cells of the user's geometry, once the cell size
        ! is known (it depends on the font the host found).
        if (.not. sized .and. views(active)%cell_w > 0.0) then
            call ui_set_window_size(nint(real(geo(1)) * views(active)%cell_w), &
                nint(real(geo(2)) * views(active)%cell_h))
            sized = .true.
        end if

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
    ! Gone at once; tearing the GPU and the programs down takes a moment.
    call ui_set_window_visible(.false.)
    do k = 1, MAX_TABS
        if (used(k)) call term_close(terms(k))
    end do
    call ui_shutdown()

contains

    subroutine wait_as_spare()
        !! Size the hidden window, then sleep until a start arrives; take its
        !! arguments, directory and environment and show the window.
        character(len=1024), allocatable :: req_args(:)
        call ui_cell_metrics(font_size, cw, ch)
        call ui_set_window_size(nint(real(geo(1)) * cw), nint(real(geo(2)) * ch))
        sized = .true.
        lfd = spare_listen(spare_path())
        if (lfd < 0) stop
        do
            conn = spare_accept(lfd)
            if (conn < 0) cycle
            call spare_receive(conn, request, got)
            if (.not. got) then
                call spare_close(conn)
                cycle
            end if
            if (spare_outdated()) then
                ! fortterm was updated since this spare started: the caller
                ! starts by itself.
                call spare_send(conn, "outdated")
                call spare_close(conn)
                call spare_close(lfd)
                call spare_unlink(spare_path())
                stop
            end if
            if (.not. spare_same_program(request)) then
                ! Another build of fortterm: it starts by itself.
                call spare_send(conn, "other")
                call spare_close(conn)
                cycle
            end if
            call spare_send(conn, "ok")
            call spare_close(conn)
            exit
        end do
        call spare_close(lfd)
        call spare_unlink(spare_path())
        call spare_apply(request, req_args, req_cwd)
        call parse_options(req_args, opts)
        if (len(opts%cwd) == 0) opts%cwd = req_cwd
        if (len(opts%title) == 0) continue
        if (opts%font_size > 0.0) font_size = opts%font_size
        spawn_spare = .true.
        if (opts%maximize) call ui_maximize_window()
        call ui_set_window_visible(.true.)
        sized = sized .or. opts%maximize
    end subroutine wait_as_spare

    subroutine open_tab(first)
        !! A new tab running the command line's program (the first tab) or
        !! the shell (later ones).
        logical, intent(in) :: first
        integer :: j
        do j = 1, MAX_TABS
            if (used(j)) cycle
            used(j) = .true.
            views(j) = terminal_view_t()
            views(j)%show_exit = opts%hold
            terms(j)%default_fg = fg
            terms(j)%default_bg = bg
            terms(j)%palette = palette
            terms(j)%own_palette = .true.
            terms(j)%cursor_color = cursor_color
            terms(j)%bold_bright = user%bold_bright
            terms(j)%cursor_shape = cursor_shape
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

    logical function env_set(name)
        character(*), intent(in) :: name
        integer :: n, st
        character(len=8) :: v
        call get_environment_variable(name, v, n, st)
        env_set = st == 0 .and. n > 0 .and. v /= "0"
    end function env_set

    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, '(i0)') i
        s = trim(buf)
    end function itoa

end program fortterm
