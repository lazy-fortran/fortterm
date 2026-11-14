module fortterm_spare
    !! The spare instance: after a start, a fortterm waits hidden with its
    !! window and GPU ready, so the next start only hands it the command line
    !! over a Unix socket and the window appears at once. The socket lives in
    !! $XDG_RUNTIME_DIR (private to the user); a spare sleeps in accept() and
    !! uses no CPU while it waits.
    use iso_c_binding
    implicit none
    private

    public :: spare_path, spare_connect, spare_listen, spare_accept, spare_send, spare_receive
    public :: spare_close, spare_unlink, spare_spawn, spare_outdated, spare_request
    public :: spare_apply

    integer(c_int), parameter :: AF_UNIX = 1, SOCK_STREAM = 1

    type, bind(C) :: sockaddr_un_t
        integer(c_short) :: family = AF_UNIX
        character(kind=c_char) :: path(108) = c_null_char
    end type sockaddr_un_t

    interface
        integer(c_int) function c_socket(domain, kind, protocol) bind(C, name="socket")
            import :: c_int
            integer(c_int), value :: domain, kind, protocol
        end function
        integer(c_int) function c_connect(fd, addr, n) bind(C, name="connect")
            import :: c_int, sockaddr_un_t
            integer(c_int), value :: fd
            type(sockaddr_un_t), intent(in) :: addr
            integer(c_int), value :: n
        end function
        integer(c_int) function c_bind(fd, addr, n) bind(C, name="bind")
            import :: c_int, sockaddr_un_t
            integer(c_int), value :: fd
            type(sockaddr_un_t), intent(in) :: addr
            integer(c_int), value :: n
        end function
        integer(c_int) function c_listen(fd, backlog) bind(C, name="listen")
            import :: c_int
            integer(c_int), value :: fd, backlog
        end function
        integer(c_int) function c_accept(fd, addr, n) bind(C, name="accept")
            import :: c_int, c_ptr
            integer(c_int), value :: fd
            type(c_ptr), value :: addr, n
        end function
        integer(c_long) function c_read(fd, buf, n) bind(C, name="read")
            import :: c_int, c_long, c_size_t, c_char
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
        end function
        integer(c_long) function c_write(fd, buf, n) bind(C, name="write")
            import :: c_int, c_long, c_size_t, c_char
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: buf(*)
            integer(c_size_t), value :: n
        end function
        integer(c_int) function c_close(fd) bind(C, name="close")
            import :: c_int
            integer(c_int), value :: fd
        end function
        integer(c_int) function c_unlink(path) bind(C, name="unlink")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function
        type(c_ptr) function c_getcwd(buf, n) bind(C, name="getcwd")
            import :: c_ptr, c_char, c_size_t
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
        end function
        integer(c_int) function c_clearenv() bind(C, name="clearenv")
            import :: c_int
        end function
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name="setenv")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function
        integer(c_size_t) function c_strlen(s) bind(C, name="strlen")
            import :: c_size_t, c_ptr
            type(c_ptr), value :: s
        end function
        type(c_funptr) function c_signal(sig, handler) bind(C, name="signal")
            import :: c_funptr, c_int
            integer(c_int), value :: sig
            type(c_funptr), value :: handler
        end function
        integer(c_int) function fortui_spawn_detached(path, arg) &
                bind(C, name="fortui_spawn_detached")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*), arg(*)
        end function
        integer(c_long) function c_readlink(path, buf, n) bind(C, name="readlink")
            import :: c_long, c_char, c_size_t
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
        end function
    end interface

contains

    function spare_path() result(path)
        !! $XDG_RUNTIME_DIR/fortterm-spare ("" without a runtime directory).
        character(len=:), allocatable :: path
        character(len=256) :: dir
        integer :: n, st
        path = ""
        call get_environment_variable("XDG_RUNTIME_DIR", dir, n, st)
        if (st /= 0 .or. n == 0 .or. n > 90) return
        path = trim(dir) // "/fortterm-spare"
    end function spare_path

    function address(path) result(a)
        character(*), intent(in) :: path
        type(sockaddr_un_t) :: a
        integer :: k
        do k = 1, min(len(path), 107)
            a%path(k) = path(k:k)
        end do
    end function address

    integer function spare_connect(path) result(fd)
        !! A connection to the waiting spare, or -1.
        character(*), intent(in) :: path
        fd = -1
        if (len(path) == 0) return
        fd = c_socket(AF_UNIX, SOCK_STREAM, 0_c_int)
        if (fd < 0) return
        if (c_connect(fd, address(path), int(c_sizeof(sockaddr_un_t()), c_int)) /= 0) then
            call spare_close(fd)
            fd = -1
        end if
    end function spare_connect

    integer function spare_listen(path) result(fd)
        !! Listen at `path` (a stale socket file is replaced), or -1.
        character(*), intent(in) :: path
        integer :: st
        fd = -1
        if (len(path) == 0) return
        st = c_unlink(path // c_null_char)
        fd = c_socket(AF_UNIX, SOCK_STREAM, 0_c_int)
        if (fd < 0) return
        if (c_bind(fd, address(path), int(c_sizeof(sockaddr_un_t()), c_int)) /= 0 .or. &
            c_listen(fd, 4_c_int) /= 0) then
            call spare_close(fd)
            fd = -1
        end if
    end function spare_listen

    integer function spare_accept(fd) result(conn)
        !! Wait (asleep) for the next start. SIGTERM and SIGINT end a waiting
        !! spare at once (SDL would turn them into events nobody reads now).
        integer, intent(in) :: fd
        type(c_funptr) :: old
        old = c_signal(15_c_int, c_null_funptr)
        old = c_signal(2_c_int, c_null_funptr)
        conn = c_accept(int(fd, c_int), c_null_ptr, c_null_ptr)
    end function spare_accept

    subroutine spare_send(fd, message)
        !! A message: its length in 8 decimal digits, then its bytes.
        integer, intent(in) :: fd
        character(*), intent(in) :: message
        character(len=8) :: head
        integer(c_long) :: w
        write (head, '(i8.8)') len(message)
        w = c_write(int(fd, c_int), head // message, int(8 + len(message), c_size_t))
    end subroutine spare_send

    subroutine spare_receive(fd, message, ok)
        integer, intent(in) :: fd
        character(len=:), allocatable, intent(out) :: message
        logical, intent(out) :: ok
        character(len=8) :: head
        integer :: n, ios
        ok = .false.
        message = ""
        if (.not. read_all(fd, head)) return
        read (head, '(i8)', iostat=ios) n
        if (ios /= 0 .or. n < 0 .or. n > 16 * 1024 * 1024) return
        deallocate(message)
        allocate(character(len=n) :: message)
        if (n > 0) ok = read_all(fd, message)
        if (n == 0) ok = .true.
    end subroutine spare_receive

    logical function read_all(fd, buf)
        integer, intent(in) :: fd
        character(*), intent(out) :: buf
        integer(c_long) :: got
        integer :: done
        done = 0
        read_all = .false.
        do while (done < len(buf))
            got = c_read(int(fd, c_int), buf(done + 1:), int(len(buf) - done, c_size_t))
            if (got <= 0) return
            done = done + int(got)
        end do
        read_all = .true.
    end function read_all

    subroutine spare_close(fd)
        integer, intent(in) :: fd
        integer :: st
        if (fd >= 0) st = c_close(int(fd, c_int))
    end subroutine spare_close

    subroutine spare_unlink(path)
        character(*), intent(in) :: path
        integer :: st
        if (len(path) > 0) st = c_unlink(path // c_null_char)
    end subroutine spare_unlink

    function exe() result(path)
        character(len=:), allocatable :: path
        character(kind=c_char, len=4096) :: buf
        integer(c_long) :: n
        path = ""
        n = c_readlink("/proc/self/exe" // c_null_char, buf, int(len(buf), c_size_t))
        if (n > 0) path = buf(1:int(n))
    end function exe

    logical function spare_outdated()
        !! The program file was replaced since this process started.
        spare_outdated = index(exe(), " (deleted)") > 0
    end function spare_outdated

    function spare_request(args) result(req)
        !! What a start hands the spare: the working directory, the arguments
        !! and the environment, as NUL-separated "cwd=", "arg=", "env=" items.
        character(*), intent(in) :: args(:)
        character(len=:), allocatable :: req
        character(kind=c_char, len=4096) :: dir
        integer :: k
        req = ""
        if (c_associated(c_getcwd(dir, int(len(dir), c_size_t)))) &
            req = "cwd=" // dir(1:index(dir, c_null_char) - 1) // c_null_char
        do k = 1, size(args)
            req = req // "arg=" // trim(args(k)) // c_null_char
        end do
        req = req // environment()
    end function spare_request

    function environment() result(items)
        !! This process's environment as "env=NAME=value" items (from
        !! /proc/self/environ, NUL-separated).
        character(len=:), allocatable :: items, raw
        integer :: u, ios, a, b, sz
        character :: c
        items = ""
        open (newunit=u, file="/proc/self/environ", access="stream", form="unformatted", &
            action="read", status="old", iostat=ios)
        if (ios /= 0) return
        sz = 0
        allocate(character(len=65536) :: raw)
        do
            read (u, iostat=ios) c
            if (ios /= 0) exit
            sz = sz + 1
            if (sz > len(raw)) raw = raw // repeat(" ", len(raw))
            raw(sz:sz) = c
        end do
        close (u)
        a = 1
        do while (a <= sz)
            b = index(raw(a:sz), c_null_char)
            if (b == 0) b = sz - a + 2
            if (b > 1) items = items // "env=" // raw(a:a + b - 2) // c_null_char
            a = a + b
        end do
    end function environment

    function c_string(p, n) result(s)
        type(c_ptr), intent(in) :: p
        integer, intent(in) :: n
        character(len=n) :: s
        character(kind=c_char), pointer :: chars(:)
        integer :: k
        call c_f_pointer(p, chars, [n])
        do k = 1, n
            s(k:k) = chars(k)
        end do
    end function c_string

    subroutine spare_apply(req, args, cwd)
        !! The request into arguments and directory; its environment replaces
        !! this process's, so the program starts as if from the caller.
        character(*), intent(in) :: req
        character(len=1024), allocatable, intent(out) :: args(:)
        character(len=:), allocatable, intent(out) :: cwd
        integer :: a, b, n, eq, st
        character(len=1024), allocatable :: grown(:)
        allocate(args(16))
        n = 0
        cwd = ""
        st = 0
        if (index(req, c_null_char // "env=") > 0 .or. req(1:min(4, len(req))) == "env=") &
            st = c_clearenv()
        a = 1
        do while (a <= len(req))
            b = index(req(a:), c_null_char)
            if (b == 0) exit
            b = a + b - 1
            select case (req(a:min(a + 3, b - 1)))
            case ("cwd=")
                cwd = req(a + 4:b - 1)
            case ("arg=")
                n = n + 1
                if (n > size(args)) then
                    allocate(grown(2 * size(args)))
                    grown(1:n - 1) = args(1:n - 1)
                    call move_alloc(grown, args)
                end if
                args(n) = req(a + 4:b - 1)
            case ("env=")
                eq = index(req(a + 4:b - 1), "=")
                if (eq > 1) st = c_setenv(req(a + 4:a + 2 + eq) // c_null_char, &
                    req(a + 4 + eq:b - 1) // c_null_char, 1_c_int)
            end select
            a = b + 1
        end do
        allocate(grown(n))
        grown = args(1:n)
        call move_alloc(grown, args)
    end subroutine spare_apply

    subroutine spare_spawn()
        !! Start the next spare, detached from this process.
        character(len=:), allocatable :: path
        path = exe()
        if (len(path) == 0 .or. index(path, " (deleted)") > 0) return
        if (fortui_spawn_detached(path // c_null_char, "--spare" // c_null_char) /= 0) continue
    end subroutine spare_spawn

end module fortterm_spare
