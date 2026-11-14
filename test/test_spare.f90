program test_spare
    !! What a start hands the spare: the program file first (a spare serves
    !! only its own build), then the directory, the arguments and the
    !! environment; and the spare reads them back.
    use iso_c_binding, only: c_null_char
    use fortterm_spare, only: spare_request, spare_apply, spare_same_program
    implicit none

    character(len=:), allocatable :: req, cwd
    character(len=1024), allocatable :: args(:)
    integer :: failures = 0

    req = spare_request([character(len=8) :: "-x", "true"])
    call check(index(req, "exe=/") == 1, "program file first")
    call check(spare_same_program(req), "same program recognized")
    call check(index(req, c_null_char // "cwd=/") > 0, "working directory")
    call check(index(req, c_null_char // "arg=-x" // c_null_char // "arg=true" // c_null_char) &
        > 0, "arguments in order")
    call check(index(req, c_null_char // "env=") > 0, "environment")
    call spare_apply(req, args, cwd)
    call check(size(args) == 2 .and. args(1) == "-x" .and. args(2) == "true", "arguments back")
    call check(len(cwd) > 0, "directory back")
    call check(.not. spare_same_program("exe=/elsewhere/fortterm" // c_null_char), &
        "another build declined")

    if (failures > 0) error stop "spare tests failed"
    print *, "fortterm spare tests passed"

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        if (.not. ok) then
            print *, "FAIL: ", what
            failures = failures + 1
        end if
    end subroutine check

end program test_spare
