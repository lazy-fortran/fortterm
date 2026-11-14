program test_settings
    !! xfce4-terminal's spellings: GDK colours with 4 hex digits per channel,
    !! Pango font names with the size in points at the end.
    use iso_fortran_env, only: int32
    use fortterm_settings, only: parse_colour, parse_font
    implicit none

    integer(int32) :: c
    logical :: ok
    character(len=:), allocatable :: family
    real :: points
    integer :: failures = 0

    call parse_colour("#aaaaaaaaaaaa", c, ok)
    call check(ok .and. c == int(z'FFAAAAAA', int32), "GDK grey")
    call parse_colour("#46468282b4b4", c, ok)
    call check(ok .and. c == int(z'FFB48246', int32), "GDK steel blue")
    call parse_colour("#1e1e1e", c, ok)
    call check(ok .and. c == int(z'FF1E1E1E', int32), "six digits")
    call parse_colour("grey", c, ok)
    call check(.not. ok, "names are not colours")
    call parse_font("More Perfect DOS VGA 15", family, points)
    call check(family == "More Perfect DOS VGA" .and. points == 15.0, "family and points")
    call parse_font("Monospace", family, points)
    call check(family == "Monospace" .and. points == 0.0, "no size")

    if (failures > 0) error stop "settings tests failed"
    print *, "fortterm settings tests passed"

contains

    subroutine check(good, what)
        logical, intent(in) :: good
        character(*), intent(in) :: what
        if (.not. good) then
            print *, "FAIL: ", what
            failures = failures + 1
        end if
    end subroutine check

end program test_settings
